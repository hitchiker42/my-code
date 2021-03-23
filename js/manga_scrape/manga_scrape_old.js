const rp = require('request-promise')
const rq = require('request')
const cheerio = require('cheerio')
const path = require('path')
const fs = require('fs')
const archiver = require('archiver')
const cli_progress = require('cli-progress')
var failed_urls = []
const website_url = 'https://mangakakalot.com/manga'
var progress_bar = null;
/*
  Usage: node 'this file' url_of_1st_chapter [directory to download to]
    Downloads the given chapter and all subsequent chapters
*/
/*
  Starting from the base page of a manga we can get the list of chapters using
  $('#chapter .chapter-list .row).map(function(i, el){
    return [$(this).attr("href"), $(this).attr("title")]
  })
  Or to simply download each chapter
  $('#chapter .chapter-list .row').each(function(i, el){
    download_chapter($(this).attr("href"))
  })


  Selectors for the things we need, these may not work for every title.
  This /should/ give us the title
  $(".rdfa-breadcrumb span[itemprop='title']:eq(1)").text()
  This /should/ give us the next chapter, if it exists, of note is that
  sometimes the class of the next button is .back instead of .next
  $(".next,.back").filter(":contains('NEXT CHAPTER')").attr("href")
  This gives us the list of images, it should always work, at least I'm more
  confident than for the other 2. This will return an array of urls.
  $(".vung-doc img") gets the html  nodes of all the images, then we map over them←
  to grab the urls.
  $(".vung-doc img").map(function(){return this.getAttribute("src")}).get()

*/
function get_title($){
  return $(".rdfa-breadcrumb span[itemprop='title']:eq(1)").text()
}
function url_basename(url, ext){
  return path.basename(new URL(url).pathname, ext)
}
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
  var options = {uri : url, transform : body => cheerio.load(body)}
  var chapter = url_basename(url)
  var chapter_num = chapter.substring(chapter.lastIndexOf('_')+1)
  console.log("Downloading chapter " + chapter_num)
  mkdir_if_not_exists(chapter)
  if(!progress_bar){
    progress_bar = new cli_progress.SingleBar({});
  } else {
    progress_bar.stop();
  }
  url = rp(options)
    .then(function($){
      var urls = $(".vung-doc img").map(function(){return $(this).attr('src')}).get()
      console.log("Downloading %d pages", urls.length);
      progress_bar.start(urls.length, 0);
      //Iteration using recursion, what is this, scheme?
      function doit(urls){
        while(urls.length > 0){
          let url = urls.pop()
          let path = chapter + '/' + chapter_num + '_' + url_basename(url)
          if(file_exists(path) && check_image(path)){ continue } //skip already downloaded images
          var msg = "Downloaded chapter " + chapter_num + " page " + url_basename(url, '.jpg')
          rq({uri : url, timeout : 1000 * 10})
            .on('error', (err) => {
              console.log(err);
              failed_urls.push(url);
              progress_bar.increment();
              doit(urls);
            })
            .pipe(fs.createWriteStream(path).on('close', () => {
              console.log(msg);
              progress_bar.increment();
              doit(urls)
            }))
          return
        }
        return download_chapter($(".next,.back").filter(":contains('NEXT CHAPTER')").attr("href"))
      }
      doit(urls)
    })
    .catch(function(err){ throw err })
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
