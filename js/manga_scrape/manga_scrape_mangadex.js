const rp = require('request-promise')
const rq = require('request')
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
//Potentially inefficent but makes sure we have proper urls and pathnames
function url_basename_to_path(url, ext){
  return path.basename(new URL(url).pathname, ext)
}
function url_basename(url){
  return new URL(url).pathname
}
function url_hostnamee(url){
  return new URL(url).hostname
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
function reader_controls($){
  return $('#content .reader-controls')
}
function reader_main($){

}
function get_page_count($){
  return $('#content').attr(data-total-pages)
}
function get_current_page($){
  return $('#content').attr(data-current-page)
}
function at_last_page($){
  return get_page_count($) == get_current_page($);
}
function get_image_url($){
  return $('#content .reader-main .reader-image-wrapper img').attr('src')
}
//Assume that $ is a reference to reader-controls
function get_title($){
  return $('#content .reader-controls .reader-controls-title a.manga-link').text();
}
function chapter_title($){
  //This can't possibly be the best way to do this, but it works
  return $('#jump-chapter :selected').text();
}
function next_chapter($){
  let next = $('.reader-controls-chapters a.chapter-link-right')
  if(next.attr('title') == "Back to manga"){
    return null
  } else {
    return next.attr('href');
  }
}
//I think this should work, it should return a promise that resolves
//when the image is downloaded, timesout or some other error is raised.
function download_image(url, dest){
  let timeout = set_abort_timeout(1000 * 10);
  fetch(url, {signal : controller.signal})
    .then(
      res => {
        const dest = fs.createWriteStream(path);
        return pipeline(res.body, dest);
      },
      err => {
        if(err.name === 'AbortError'){
          console.log("Request Aborted")
        }
        console.log(err);
        failed_urls.push(url);
      }
    )
    .finally(() => {
      clearTimeout(timeout);
    })
}
function get_image_url_from_page_url(url){
  cheerio_fetch(url)
  .then(function($){
    return get_image_url($);
  })
}
function download_chapter(url){
  if(!url){ return; }
  cheerio_fetch(url)
    .then(function($){
      let controls = reader_controls($);
      let title = get_title(controls);
      let page_count = get_page_count(controls);
      let hostname = new URL(url).hostname;
      var urls = $('#content .reader-main .reader-page-bar .notches .notch')
          .map(function(){ return hostname + '/' + $(this).attr('data-page') }).get()
      //current page should usually be 1, but this dosen't really hurt
      progress_bar.start(page_count - get_current_page($), 0)
      function doit(urls){
        while(urls.length > 0){
          let url = urls.pop();
          //This assumes that the 'basename' of the url is just a number
          let path = chapter_name + '/' + url_basename(url).padStart(3, '0');
          download_chapter(url, path)
            .then(() => {
              progress_bar.increment();
              doit(urls);
            })
        }
        progress_bar.stop();
        //download the next chapter, it's a bit weird to put this here, control flow wise,
        //but it's much eaiser to do it this way
        return download_chapter($)
      }
      doit(urls);
    })
}



