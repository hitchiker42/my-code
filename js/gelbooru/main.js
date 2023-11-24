import { createHash } from "node:crypto"
import * as fs from "node:fs/promises"
import { pipeline } from "node:stream/promises"
import { Readable } from "node:stream"
import * as path from "node:path"
import * as util from "node:util"
import process from "node:process"
import { json as streamToJSON } from "node:stream/consumers"
import { default as cliProgress } from "cli-progress"

//import * as webStream from "node:stream/web"
/*
  Format of data returned by gelbooru (json formatted)
  {"@attributes": {limit: (integer, max 100),
                   offset: (integer, page offset),
                   count: (integer, total number of posts)},
    "post": [array of post objects]},
  post object is:
  {id: (integer, post id}, created_at: (string, date),
  width: integer, height: integer, md5: string,
  image: (string, image filename), rating: (string, how explicit is it),
  tags: (string, space seperated list of tags), file_url: (string, file location)}
  there are several other fields, but they aren't important

  date format is: %a %b %e %T %z %Y (day abbr, month abbr, day of month, HH:MM:SS, tz, year)
 */
//Generate the md5sum of the given buffer and return it as a string
function md5Hash(buffer){
  const hash = createHash("md5")
  hash.update(buffer)
  return hash.digest("hex")
}
function generateAPI_URL({ limit = 100, pid = 0,
                           tags = "", id = null, json = true} = {}){
  const baseURL = "https://gelbooru.com/index.php?page=dapi&s=post&q=index"
  return baseURL + `&limit=${limit}&pid=${pid}&json=${json ? 1 : 0}` +
    ((id != null) ? `&id=${id}` : `&tags=${tags}`)  //If id is given ignore tags
}
async function sleep(ms){
  return new Promise((resolve,reject) => setTimeout(resolve, ms))
}
async function fetchPosts(tags, pid = 0){
  let url = generateAPI_URL({tags: tags, pid: pid})
//  console.log(`search url ${url}`)
  let response = await fetch(url)
  return await streamToJSON(response.body)
}
async function fetchToFile(url, destFile){
  let fd = await fs.open(destFile, "w")
  let writeStream = fd.createWriteStream()
  let response = await fetch(url)
  let readStream = Readable.fromWeb(response.body)
  return pipeline(readStream, writeStream)
}
async function gelbooruFetchAllPosts(tags){
  let pid = 0
  let response = await fetchPosts(tags, pid)
  const count = response["@attributes"].count
  if(count == 0){ return [] }
  let posts = response["post"]
  const bar = new cliProgress.Bar()
  bar.start(count)
  do {
    response = await fetchPosts(tags, pid)
    posts = posts.concat(response["post"])
    bar.increment(100)
  } while ((++pid * 100) < count)
  bar.stop()
  return posts
}
//Fetch metadata for the image with the given md5sum,
//If the image can't be found display an error message and
//return an empty placeholder value.
async function gelbooruFetchMetadata(md5){
  let posts = await fetchPosts(`md5:${md5}`)
  if(posts.post.length == 0){
    console.error(`Could not find post with md5sum ${md5}`)
    return {tags:[]}
  } else {
    return posts.post[0]
  }
}
async function downloadMetadata(dirname, metadata){
  let dir = await fs.opendir(dirname)
  for await (let dirent of dir){
    let filename = path.join(dirent.path, dirent.name)
    let md5 = md5Hash(await readFile(filename))
    if(metadata[md5]){
      continue
    } else {
      metedata[md5] = gelbooruFetchMetadata(md5)
    }
  }
}
async function downloadImages(tags, metadata,
                              dir = "images", waitTime = 100){
  console.log("Fetching posts")
  let posts = await gelbooruFetchAllPosts(tags)
  if(posts.length == 0){
    console.log(`No posts found for tags ${tags}`)
    return
  }
  await fs.mkdir(dir, {recursive: true})
  const bar = new cliProgress.Bar()
  bar.start(posts.length, 0)
  for(let post of posts){
    bar.increment()
    let {image, file_url, md5} = post
    if(await fileExists(path.join(dir, image))){
      continue
    }
    metadata[md5] = post
    await fetchToFile(file_url, path.join(dir, image))
    sleep(waitTime)
  }
  bar.stop()
}
async function fileExists(filename){
  try {
    let st = await fs.stat(filename)
    return st.isFile() && st.size > 0
  } catch(err){
    if(err.code == "ENOENT"){
      return false
    } else {
      throw err
    }
  }
}
const cliOptions = {
  tags: { type: "string", short: "t" },
  directory: { type: "string", short: "d"},
  metadata: { type: "string", short: "m", default: "metadata.json"},
  test: { type: "boolean", short: "t", default: false}
}
async function main(){
  let { values: options , positionals: args } =
      util.parseArgs({options: cliOptions, allowPositionals: true})
  let metadata = {}
  try {
    metadata = await fs.readFile(options.metadata)
  } catch(err){
    if(err.code != "ENOENT"){
      throw err
    }
  }
  if(options.test){
    options.tags = "nyamota"
    options.directory = "images"
  }
  try {
    if(options.tags){
      await downloadImages(options.tags, metadata, options.directory)
    } else if(options.directory){
      await downloadMetadata(options.directory, metadata)
    } else {
      console.log("Must provide either tags or a directory to download metadata for")
      process.exitCode = 1
    }
  } catch(err){
    console.log(err)
    process.exitCode = 1
  } finally {
    if(!process.exitCode){ //either 0 or just undefined, both work
      await fs.writeFile(options.metadata, JSON.stringify(metadata))
    }
  }
}
await main()
