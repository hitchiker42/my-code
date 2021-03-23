const fetch = require('node-fetch')
const cheerio = require('cheerio')
const path = require('path')
const fs = require('fs')
const archiver = require('archiver')
const cli_progress = require('cli-progress')
const { pipeline } = require('stream/promises')
const AbortController = require('abort-controller')
var failed_urls = []
const website_url = 'https://manganelo.com/manga'
var progress_bar = new cli_progress.SingleBar({});
//Not sure if this should be global or not
const controller = new AbortController();
function set_abort_timeout(timeout){
  return setTimeout(
    () => { controller.abort(); },
    timeout, //not sure about this extra comma, but it was in the example
  )
}
function cheerio_fetch(url, options){
  return fetch(url, options).then(res => {
    if(res.ok){
      return cheerio.load(res.body)
    } else {
      throw new Error("Got error code ${res.status}")
    }
  })
}
/*
  Usage: node 'this file' url_of_1st_chapter [directory to download to]
    Downloads the given chapter and all subsequent chapters
*/
/*
  Much nicer organization compared to mangakakalot
  $(".container-chapter-reader") gives a list of the images for the current chapter
  $(".navi-change-chapter-btn") has the buttons to go forward/backward a chapter
  //Bit more complicated, this gets a list of chapter numbers (actually strings)
  $(".panel-navigation .navi-change-chapter").first().children().map((i,el) => el.getAttribute("data-c"))
  //then to actually make the urls we append the chapter number to url_and_chapter_number(url)[0]  
*/
/*
  As far as I can tell the panel-breadcrumb has 3 'a' elements,
  'Read Manga Online', "<Manga Title>", "Chapter <chapter number>"
  So this should get the title, but it could change.
*/
function get_title($){
  return $(".panel-breadcrumb a")[1].text;
}
function url_basename(url, ext){
  return path.basename(new URL(url).pathname, ext)
}
function url_and_chapter_number(url){
  let idx = url.lastIndexOf("_")
  return [url.substring(0, idx+1), url.substring(idx+1)];
}
//TODO: Make sure this actually works
function check_image(pathname){
  var fd = fs.openSync(pathname)
  var head = Buffer.allocUnsafe(3)
  var tail = Buffer.allocUnsafe(3)
  // const jpeg_head = Buffer.from('ffD8', 'hex')
  // const jpeg_tail = Buffer.from('ffD9', 'hex')
  // const png_head = Buffer.from('8950', 'hex')
  // const png_tail = Buffer.from('6082', 'hex')
  var sz = fs.statSync(pathname).size
  fs.readSync(fd, head, 0, 3, 0)
  fs.readSync(fd, tail, 0, 3, sz - 3)
  if(head.toString('hex').includes('ffd8')){
    return (tail.toString('hex').includes('ffd9'));
  } else if(head.toString('hex').includes('8950')){
    return (tail.toString('hex').includes('6082'))
  } else {
    return false;
  }
}
function file_exists(path){
  try {
    (fs.accessSync(path, fs.constants.W_OK | fs.constants.R_OK))
  } catch(err) {
    if(err.code !== "EACCESS" && err.code !== "ENOENT"){
      throw err
    } else {
      return false;
    }
  }
  return true;
}
function mkdir_if_not_exists(path, options){
  try {
    fs.mkdirSync(path, options)
  } catch(err){
    if(err.code !== "EEXIST" || !fs.statSync(path).isDirectory()){
      throw err
    }
  }
}
function create_zip_archive(filename, options){
  let out = fs.createWriteStream(filename);
  let archive = archiver("zip", options);
  output.on('close', function(){console.log("Wrote archive %s", filename)})
  //For now just abort on error, ultimately we should just close the archive and continue running
  archive.on('warning', function(err){throw err;})
  archive.on('error', function(err){throw err;})
  archive.pipe(out);
  return archive
}

function download_chapter(url){
  if(!url){ return; }
  var chapter = url_basename(url)
  var chapter_num = chapter.substring(chapter.lastIndexOf('_')+1)
  mkdir_if_not_exists(chapter)
  console.log("Downloading chapter " + chapter_num)
  url = cheerio_fetch(url)
    .then(function($){
      var urls = $(".container-chapter-reader img").map(function(){return $(this).attr('src')}).get()
      console.log("Downloading %d pages", urls.length);
      progress_bar.start(urls.length, 0);
      //Iteration using recursion, what is this, scheme?
      function doit(urls){
        while(urls.length > 0){
          let url = urls.pop()
          let path = chapter + '/' + chapter_num + '_' + url_basename(url)
          if(file_exists(path) && check_image(path)){
            progress_bar.increment();
            continue
          } //skip already downloaded images
          let timeout = set_abort_timeout(1000 * 10);
          //var msg = "Downloaded chapter " + chapter_num + " page " + url_basename(url, '.jpg')
          fetch(url, {signal : controller.signal})
            .then(
              res => {
                const dest = fs.createWriteStream(path);
                return pipeline(res.body, dest); //returns a promise
              },
              err => {                
                console.log(err);
                failed_urls.push(url);
              }
            )
            .finally(() => {
              progress_bar.increment();
              //reset timeout here
              doit(urls);
            })
          return undefined //makes the linter happy
        }
        progress_bar.stop();
        return download_chapter($(".panel-navigation .navi-change-chapter-btn .navi-change-chapter-btn-next").attr("href"))
      }
      doit(urls)
    })
}
function main(){
  let argv = process.argv
  let args = process.argv.slice(2)
  if(args.length < 1){
    console.log("usage %s %s url [directory]", argv[0], argv[1])
    return 1;
  }
  var url = args[0]
  if(args[1]){
    mkdir_if_not_exists(args[1], {recursive : true})
    process.chdir(args[1])
  }
  download_chapter(url)
  //var archive = archiver('zip')
  //archive.glob('*.jpg')
  //archive.finalize()
}
main()
