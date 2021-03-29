/*
  Code to download and managage a local database of gelbooru image tags
*/
const fetch = require('node-fetch')
const level = require('level')
const cli_progress = require('cli-progress')
const fs = require('fs')
const path = require('path')
const assert = require('assert')

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
//Return a boolean indiciating if the given key exists in the database
async function db_exists(db, key){
  return await db.get(key)
      .then(() => true)
      .catch((err) => { if(err.notFound){ return false } else { throw err} })
}
//Return the value for the given key or a null value if not found (defaults to undefined)
async function db_find_or_null(db, key, nil = undefined){
  return await db.get(key)
      .then((val) => val)
      .catch((err) => { if(err.notFound){ return nil } else { throw err} })
}
async function db_update(db, key, fn){
  let val = await db.get(key)
  return await db.put(key, fn(val))
}
//Add the given key/value pair to db, while also incrementing the 'size' key
async function db_add(db, key, val){
  assert.notStrictEqual(key, "")
  return await db.put(key, val).then(() => db_update(db, 'size', x => x + 1))
}  
//Return a list of the files in the given directory sorted by modification, newest first
function sort_dir_by_modtime(dirname){
  let filenames = fs.readdirSync(dirname)
  //Avoid calling stat multiple times per file
  let stats = filenames.map((name,idx) => [idx, fs.statSync(path.join(dirname, name))])
  stats.sort((a, b) => a[1].mtimeMs > b[1].mtimeMs)
  return stats.map(x => filenames[x[0]])
}
//Set the value of key in map to the result of calling fn with its current value,
//or a given default if not found
function map_update(map, key, fn, deflt){
  let val = map.get(key)
  if (val === undefined) { val = deflt }
  map.set(key, fn(val))
}
//Return a map containing the count each tag found in the given db
async function count_tags(db){
  let map = new Map()
  let inc_key = (key) => map_update(map, key, x => x + 1, 0)
  
  /*
  let db_size = await db.get('size')
  let stream = db.createValueStream();
  let v = null
  const progress_bar = new cli_progress.SingleBar({etaBuffer: 25});
  progress_bar.start(db_size, 0)
  while((v = stream.read())){
    for (let tag of v){
      inc_key(tag)
    }
    progress_bar.increment()
  }
  progress_bar.stop();
  */
  return map
}
function write_tags(tags, filename){
  let ws = fs.createWriteStream(filename)
  for([key, value] of tags){
    ws.write(`${key}:${value}\n`)
  }
  ws.end();
}
    
//Download tag info for files in dir, assumes files are in the format 'md5sum'.'ext' and
//only download data for files newer than the newest file in the database. 
//Displays a progress bar than only works if all files currently in the db are from dir
async function update_tags(dir, db){
  let files = sort_dir_by_modtime(dir)
  const progress_bar = new cli_progress.SingleBar({});
  let db_size = await db.get('size')
  progress_bar.start(files.length - db_size, 0)
  for (file of files){
    let md5 = strip_ext(file)
    if (await db_exists(db, md5)) { break }
    //console.log(`fetching data for post ${md5}`)
    await fetch_post(md5)
      .then(val => {
        db_add(db, md5, val['tags'])        
      })
      .catch(err => {
        if(err.type == 'invalid-json'){
          console.log(`could not find file with md5sum ${md5}`)
        } else {
          throw err
        }
      })
    progress_bar.increment()
  }
  progress_bar.stop();
}
//Download tag info for files in dir, assumes files are in the format 'md5sum'.'ext'
//Will always download tags for all files in the given directory, will also reset db size
//to ensure consistancy
async function fetch_tags(dir, db){
  let files = fs.readdirSync(dir)
  let failed_files = [];
  const progress_bar = new cli_progress.SingleBar({etaBuffer: 25});
  await db.put('size', 0)
  progress_bar.start(files.length, 0)
  for (file of files){
    let md5 = strip_ext(file)
//    console.log(`fetching data for post ${md5}`)
    await fetch_post(md5)
      .then(val => {
        db_add(db, md5, val['tags'])        
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
async function main(){
  let argv = require('minimist')(process.argv.slice(2))
  let args = argv._ //underscore gives all not option args
  let dir = args.length > 0 ? args[0] : process.cwd()
  const db = level(db_name, {valueEncoding: 'json'}) //will throw on error
  if(!(await db_exists(db, 'size'))){
    db.put('size', 0)
  }
  console.log(`Downloading tags for files in ${dir}`);
  await fetch_tags(dir, db);
//  console.log('Finished Downloading tags, creating list of tag counts')
//  let tag_counts = await count_tags(db)
//  write_tags(tag_counts, 'tag_info')
}
main()
