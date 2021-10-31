#!/usr/bin/env node
/*
  Code to download and managage a local database of gelbooru image tags
*/
const fetch = require('node-fetch')
const cli_progress = require('cli-progress')
const fs = require('fs')
const path = require('path')
const assert = require('assert')
const dirty = require('dirty')
const parse_args = require('minimist')

const db_name = 'gelbooru.db'
const base_url='https://gelbooru.com'
const auth='&api_key=anonymous&user_id=9455'
const post_api='/index.php?page=dapi&s=post&q=index'
/*
  Api Info
   -Possible query options: limit(max 100),pid(page id, based on limit), tags, cid(change id, unix timestamp), id, json
   -Tags is used to actally perform search, pid/limit are used to paginate results, id & cid are of limited use
   -Results can be json or xml, default to xml, add json=1 to query to get json
   -json results are a json array of posts, xml is of the form <posts count offset><post `attributes`/></posts>
*/
//remove the trailing extension from a filename, since there's no single funtion to do this in the path module
function strip_ext(str){
  return str.substring(0, str.lastIndexOf('.'))
}
//fetch a post given a md5 sum, returns post info as a json object
async function fetch_post(md5){
  let search_url = `${base_url}${post_api}&tags=md5:${md5}&json=1${auth}`
  let result = await fetch(search_url).then(res => res.json())
  return result[0]
}
function decode_tag(tag){
  return decodeURIComponent(tag).replaceAll('&#039',"'")
}
function promise_on_event(db, event){
  return new Promise((resolve, reject) => {
    db.on(event, () => resolve(db))
  })
}
function db_open(filename){
  return new Promise((resolve, reject) => {
    let db = dirty(filename)
    db.on('error', (err) => { console.log(err)} )
    db.on('load', () => resolve(db))
  })
}
//Return a boolean indiciating if the given key exists in the database
function db_exists(db, key){
  return (db.get(key) !== undefined)
}
//Return the value for the given key or a null value if not found (defaults to undefined)
function db_find_or_null(db, key, nil = undefined){
  let ret = db.get(key);
  return key === undefined ? nil : key;
}
function db_update(db, key, fn){
  db.update(key)
}
//Add the given key/value pair to db, while also incrementing the 'size' key
function db_add(db, key, val){
  db.set(key, val)
}
//Return a list of the files in the given directory sorted by modification, oldest first
function sort_dir_by_modtime(dirname){
  let filenames = fs.readdirSync(dirname)
  //Avoid calling stat multiple times per file
  let stats = filenames.map((name,idx) => [idx, fs.statSync(path.join(dirname, name))])
  stats.sort((a, b) => a[1].mtimeMs < b[1].mtimeMs)
  return stats.map(x => filenames[x[0]])
}
//If filename == "-" returns process.stdout otherwise creates and returns
//an output stream for the given filename
function get_output_stream(filename){
  if(filename == "-"){
    return process.stdout
  } else {
    return fs.createWriteStream(filename)
  }
}
//Set the value of key in map to the result of calling fn with its current value,
//or a given default if not found
function map_update(map, key, fn, deflt){
  let val = map.get(key)
  if (val === undefined) { val = deflt }
  map.set(key, fn(val))
}
//Return a map containing the count each tag found in the given db
function count_tags(db){
  let map = new Map()
  let inc_key = (key) => map_update(map, key, x => x + 1, 0)
  let db_size = db.size()
  const progress_bar = new cli_progress.SingleBar({etaBuffer: 25});
  progress_bar.start(db_size, 0)
  for(let v of db._data.values()){
    let tags = v.split(" ")
    for (let tag of tags){
      inc_key(tag)
    }
    progress_bar.increment()
  }
  progress_bar.stop();
  return map
}
//Create a new hash table mapping md5 sums to a Set containing the tags
//for that image
function create_tag_sets(db){
  let map = new Map()
  let make_set = (v) => new Set(v.split(" "))
  for(let [md5, tagstr] of  db._data.entries()){
    map.set(md5, new Set(tagstr.split(" ")))
  }
  return map
}
//Return a number from 0-1 indicating how similar the sets of tags
//x and y are.
//let n be x.size, and m be y.size this returns
//((n+m)-d)/(n+m) where d is the number of differing tags
//As a special case subset_score is returned if y is a subset of x,
//passing 0 for subset_score will return the normal score instead.
function tag_similarity(x, y, subset_score = 0.9){
  if(x.size < y.size){
    return tag_similarity(y, x)
  }
  let subset = true
  let diff = new Set(x)
  for (let elem of y) {
    if (diff.has(elem)) {
      diff.delete(elem)
    } else {
      diff.add(elem)
      subset = false
    }
  }
  return (subset && subset_score ? subset_score
          (x.size + y.size - diff.size) / (x.size + y.size))
}
//Modifies the tags argument, May change to making a copy of it internally
function find_similar_images(tags, threshold = 0.95){
  let similar = []
  const progress_bar = new cli_progress.SingleBar({etaBuffer: 25});
  progress_bar.start(tags.size, 0)
  for([md5_1, tags_1] of tags.entries()){
    tags.delete(md5_1)
    for([md5_2, tags_2] of tags.entries()){
      if(tag_similarity(tags_1, tags_2) >= threshold){
        similar.push([md5_1, md5_2])
      }
    }
    progress_bar.increment();
  }
  progress_bar.stop()
  return similar
}
function write_dups(tags, filename, threshold){
  let ws = get_output_stream(filename)
  let dups = find_similar_images(tags, threshold)
  //may as well sort them
  dups.sort((a,b) => a[0] > b[0])
  for(dup of dups){
    ws.write(`${dup[0]} ${dup[1]}\n`)
  }
}
//Given a map tags of tag->count write the tags to filename in decending order of count.
function write_tags(tags, filename){
  let ws = get_output_stream(filename)
  let tags_sorted = [...tags].sort((a,b) => b[1] - a[1])
  for([key, value] of tags_sorted){
    ws.write(`${key}:${value}\n`)
  }
  //ws.end() //Could be stdout, probably don't want to close that
}
function find_md5_file(md5, dir){
  for(let ext of ['.jpg','.png','.jpeg']){
    let filename = md5 + ext
    if(fs.statSync(path.join(dir, filename), {throwIfNoEntry: false})){
      return filename;
    }
  }
  return undefined
}
function create_file_links(basedir, newdir, files){
  fs.mkdirSync(newdir);
  for(let file of files){
    //No need to create the links synchronously
    fs.link(path.join(basedir, file), path.join(newdir, file), (err) => undefined)
  }
}
//Create a directory containing links to all files with the given tag,
//the new directory name defaults to the same as the tag.
function create_tag_dir(db, basedir, tag, newdir = tag){
  fs.mkidrSync(newdir)
  for(let [md5, tagstr] of db._data.entries()){
    let tags = tagstr.split(' ');
    if(!tags.includes(tag)){ continue }
    let filename = find_md5_file(md5, basedir)
    if(!filename){
      console.log(`Cound not find file for md5sum ${md5} in ${dir}`)
      continue
    }
    fs.link(path.join(basedir, filename), path.join(newdir, filename), (err) => undefined)
  }
}
/*
FIXME: Storing missing files as a key in the database is a horrible hack
FIXME: Just iterate over all the files and skip ones we already have
*/
async function update_tags(dir, db){
  //files is sorted oldest to newest, that way if the update is
  //interrupted everything will still work
  let files = sort_dir_by_modtime(dir)
  let db_size = db.size()
  let missing_files = new Set(db.get("missing_files") ? db.get("missing_files").split(" ") : [])
  let update_count = files.length - (db_size + missing_files.size);
  console.log(`Updating ${update_count} files`)
  if(db_size == (files.length - missing_files.size)){
    console.log("Database is up to date")
    return;
  }
  let new_files = files.slice(files.length-(update_count+1))
  let dups = 0, missing = 0;
  const progress_bar = new cli_progress.SingleBar({etaBuffer: 25});
  progress_bar.start(update_count, 0)
  for (file of new_files){
    let md5 = strip_ext(file)
    if (missing_files.has(file)){
      progress_bar.increment()
      continue
    }
    if (db_exists(db, md5)) {
      //console.log(`\nFound duplicate md5 sum ${md5}`);
      dups++;
      progress_bar.increment();
      continue
    }
    await fetch_post(md5)
      .then(val => {
        db_add(db, md5, decodeURIComponent(val['tags']))
      })
      .catch(err => {
        if(err.type == 'invalid-json'){
          console.log(`\ncould not find file with md5sum ${md5}`)
          missing_files.add(file)
          missing++;
        } else {
          throw err
        }
      })
    progress_bar.increment()
  }
  progress_bar.stop();
  db.set("missing_files", [...missing_files].join(" "))
  console.log(`Found ${dups} duplicate md5 sums and could not find tags for ${missing} files`);
}
//Download tag info for files in dir, assumes files are in the format 'md5sum'.'ext'
//Will always download tags for all files in the given directory, will also reset db size
//to ensure consistancy
async function build_tags(dir, db){
  let files = fs.readdirSync(dir)
  let failed_files = [];
  const progress_bar = new cli_progress.SingleBar({etaBuffer: 25});
  progress_bar.start(files.length, 0)
  for (file of files){
    let md5 = strip_ext(file)
//    console.log(`fetching data for post ${md5}`)
    await fetch_post(md5)
      .then(val => {
        db_add(db, md5, decodeURIComponent(val['tags']))
      })
      .catch(err => {
        if(err.type == 'invalid-json'){
          console.log(`\ncould not find file with md5sum ${md5}`)
          failed_files.push(file)
        } else {
          throw err
        }
      })
    progress_bar.increment()
  }
  progress_bar.stop();
  db.set(db, "missing_files", failed_files)
}
function unescape_tags(db){
  for(key of db._data.keys()){
    db.update(key, decodeURIComponent)
  }
}
async function download_command(db, args){
  console.log(`Downloading tags for files in ${args.dir}`);
  if(args.update || !args.rebuild){
    console.log('Updating database');
    await update_tags(args.dir, db);
  } else {
    assert(args.rebuild === true)
    console.log('Rebuilding database');
    await build_tags(args.dir, db)
  }
  if(args.count){
    let filename = typeof(args.count) == 'string' ? args.count : '-'
    write_tags(count_tags(db), filename)
  }
}
async function count_command(db, args){
  let filename = (args.output ? args.output :
                  args._.length > 1 ? args._[1] : '-')
  if(args.update){
    console.log('Updating database');
    await update_tags(args.dir, db);
  }
  write_tags(count_tags(db), filename)
}
async function dups_command(db, args){
  let filename = (args.output ? args.output :
                  args._.length > 1 ? args._[1] : '-')
  if(args.update){
    console.log('Updating database');
    await update_tags(args.dir, db);
  }
  return write_dups(create_tag_sets(db), filename, args.threshold)
}
async function main(){
  let name = process.argv[1]
  let usage = `${name} [count | duplicates | update | download | help] [options]\n` +
      "\commands:\n" +
      "\tdownload|update|rebuild: Download tag information, download and update will only download tags for new files while rebuild will download all tags\n" +
      "\tcount: Print a list of the occurances of each tag, to a file if given otherwise to stdout\n" +
      "\tduplicates: Print a list of files with similar tags\n" +
      "\thelp: Print this help text and exit\n" +
      "options:\n" +
      "\td|dir[ectory]: The directory containing the images\n" +
      "\to|output: File to use as output if applicable\n"+
      "\tupdate: Perform the update command before the given command\n" +
      "\tt|threshold: The similarity threshold for the duplicates command"
  let args = parse_args(process.argv.slice(2),
                        {boolean: ['count', 'update', 'rebuild', 'help'],
                         string: ['output'],
                         alias: {'dir' : ['directory', 'd'],
                                 'threshold' : ['t'],
                                 'output' : ['o']},
                         default: {rebuild: false}})
  if(args.help){
    console.log(usage);
    return;
  }
  args.dir = args.dir ? args.dir : process.cwd();
  let db = await db_open(db_name)
  switch(args._[0]){
    case 'count':
      return count_command(db, args)
    case 'dups':
    case 'duplicates':
      return dups_command(db, args)
    case "help":
      console.log(usage)
      return
    case 'rebuild':
      args.rebuild = true
      fs.truncateSync(db.path, 0)
      db = await db_open(db_name)
      return download_command(db, args)
    case 'update':
      args.update = true
      return download_command(db, args)
    case 'download':
    default:
      return download_command(db, args)
  }
}
if(require.main === module){
  main()
}
/*
//Download tag info for files in dir, assumes files are in the format 'md5sum'.'ext'
//If update is true only download data for files newer than the newest file in the database.
//Displays a progress bar than only works if all files currently in the db are from dir, or
//update is false.
async function download_tags(dir, db, update = true){
  let files = fs.readdirSync(dir)
  let failed_files = [];
  const progress_bar = new cli_progress.SingleBar({etaBuffer: 25});
  let test = (x => false)
  if(update){
    let db_size = db.size()
    if(db_size == files.length){
      console.log("Database is up to date")
      return;
    }
    let test = (x => db_exists(db, x))
    progress_bar.start(files.length - db_size, 0)
  } else {
    progress_bar.start(files.length, 0)
  }
  for (file of files){
    let md5 = strip_ext(file)
    if(test(md5)){ break }
    await fetch_post(md5)
      .then(val => {
        db_add(db, md5, decode_tag(val['tags']))
      })
      .catch(err => {
        if(err.type == 'invalid-json'){
          console.log(`\ncould not find file with md5sum ${md5}`)
          failed_files.push(file)
        } else {
          throw err
        }
      })
    progress_bar.increment()
  }
  progress_bar.stop();
}
*/
