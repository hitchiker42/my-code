//This should not have been written in javascript, but its what I'm most familiar with these days.
import {default as yaml } from "js-yaml"
import {default as jszip } from "jszip"
import {default as cliProgress} from "cli-progress"
import { readFile, writeFile, opendir, mkdir, symlink, rm } from 'node:fs/promises';
import * as process from "node:process"
import * as path from "node:path"
import { parseArgs } from "node:util"

//for debugging
import * as repl from "node:repl"

const metadataFilename = "metadata.json"
const progressBarFormat = "progress [{bar}] {percentage}% | {value}/{total} | {filename}"
const zipExts = ['.zip', '.cbz']
//For files with no release info set the release date to 2000-01-02 as a failsafe.
//For some reason setting it as 2000-01-01 leaves the year as 1999 after rounding
const defaultReleaseDate = new Date("2000-01-02").getTime()
/*
  Info in yaml file:
  Artist: Array
  Description: String
  Magazine: Array
  Pages: Integer
  Parody: Array
  Publisher: Array
  Released: Integer //unix timestamp I assume
  Tags: Array
  Thumbnail: Integer
  Title: String
  URL: String
*/
function stripExt(str){
  let idx = str.lastIndexOf(".")
  return ((idx > 0) ? str.slice(0, idx) : str)
}
/*
  It'd be nice to be able to use the URL to scrape the webpage for the release date,
  but the site is generated on the fly, so I'd need to execute scripts and it also detects bots.
 */
function findReleaseDate(info){
  const date_regex = /(20\d\d)([-_]\d\d)?/
  const date_match = info.Magazine[0]?.match(date_regex)
  if(!date_match){
    return defaultReleaseDate / 1000
  } else {
    return new Date(date_match[1] + (date_match[2] ?? "-01")).getTime() / 1000
  }
}  
//Takes a string, a metadata object and optionally a filename.
//Parses the string as yaml and adds it to the metadata object.
function parseYAML(yaml_text, metadata, filename){
  let info = yaml.load(yaml_text)
  info.filename = filename
  //If no release date given try to get an estimate from the magizine title, otherwise set a default value
  if(info.Released == undefined){
    info.Released = findReleaseDate(info)
  }
  info.Year = new Date(info.Released * 1000).getFullYear()
  metadata.info[info.Title] = info
  for(let tag of info.Tags){
    metadata.tags[tag] = (metadata.tags[tag] || 0) + 1
  }
  //It's exceedingly rare, but there are some titles with multiple artists
  for(let artist of info.Artist){
    metadata.artists[artist] = (metadata.artists[artist] || 0) + 1
  }
  metadata.files[path.basename(filename)] = filename    
}
//Extracts info.yaml from pathname (a zip file), parses it and adds
//it into the given metadata variable.
async function parseYAMLFromZip(filename, metadata){
  let zipfile;
  try {
    zipfile = await jszip.loadAsync(await readFile(filename))
  } catch(err){
    console.error(`Error reading ${filename} as a zipfile: ${err}`)
    return false
  }
  let yaml_zipfile = zipfile.file("info.yaml")
  if(!yaml_zipfile){
    metadata.noInfo.push(path.basename(filename))
    return false
  }
  let yaml_text = await yaml_zipfile.async("string")
  parseYAML(yaml_text, metadata, filename)
  return true
}
//Try to create a link to target named linkname, if linkname already exists
//no error is thrown.
async function symlinkIgnoreExisting(target, linkname){
  symlinkExisting(target, linkname, "ignore")
}
async function symlinkExisting(target, linkname, how){
  if(how == "remove" || how == "replace"){
    await rm(linkname, {force: true})
  }
  try {
    await symlink(target, linkname)
  } catch(err){
    if(!(err.code == "EEXIST" && how == "ignore")){
      throw err
    }
  }
}
async function readMetadataFile(){
  let metadata = {artists: {}, tags: {}, info: {}, files: {}, noInfo: []}
  try {
    let text = await readFile(metadataFilename)
    metadata = JSON.parse(text)
  } catch(err) {
    if(err.code != "ENOENT"){
      throw err
    }
  }
  return metadata
}
async function parseMetadata(dirs, replace = false){
  let metadata = await readMetadataFile()
  let files = []
  let dups = []
  //Loop over all directories and collect the names of all the files to process
  //This is the easiest way to make a progress bar work.
  for(let dir_path of dirs){
    const dir = await opendir(dir_path)
    for await (const dirent of dir){
      //This will make symbolic links fail, dunno if that's good or not
      if(!dirent.isFile()){
        continue
      }
      let pathname = path.join(dirent.path, dirent.name)
      if(metadata.files[dirent.name] && !replace){
        if(metadata.files[dirent.name] != pathname){
          dups.push([pathname, metadata.files[dirent.name]])
        }
        continue
      }
      files.push(path.join(dirent.path, dirent.name))
    }
  }
  for(let [dup, existing] of dups){ //noop if dups is empty    
    console.log(`Found duplicate file:\n\tduplicate: ${dup}`+
                `\n\toriginal : ${existing}`)
  }
  const bar = new cliProgress.Bar({format: progressBarFormat})
  bar.start(files.length, 0, {filename: ""})
  for(let filename of files){
    bar.increment(1, {filename: path.basename(filename)})
    await parseYAMLFromZip(filename, metadata)
  }
  bar.stop()
  return metadata
}
//Create a symbolic link to target named link relative to the current directory
//i.e makeRelativeSymlink("a/b", "c/d"), will create a link "c/d" with the
//target "../../a/b"
async function makeRelativeSymlink(target, linkname, how = "ignore"){
  //Man, symlinks confuse the hell out of me for some reason
  let relativePath = path.relative(path.dirname(linkname), target)
  return symlinkExisting(relativePath, linkname, how)
}
async function makeSymlinks(metadata, replace = false){
  let how = (replace ? "replace" : "ignore")
  //make  sure directories exist
  for (let tag of Object.keys(metadata.tags)){
    await mkdir(path.join("all_tags", tag),  {recursive : true})
  }
  for (let artist of Object.keys(metadata.artists)){
    await mkdir(path.join("all_artists",artist),  {recursive : true})
  }
  await mkdir("all_by_date", {recursive: true})
  for (let year of Array.from({length: (2023-2015)+1}, (_,i) => (i + 2015))) {
    await mkdir(path.join("all_by_year", String(year)), {recursive: true})
  }
  await mkdir(path.join("all_by_year", "2000"), {recursive: true}) //For released with no date
  let count = Object.entries(metadata.info).length
  const bar = new cliProgress.Bar({format: progressBarFormat})
  bar.start(count, 0, {filename: ""})
  for (let info of Object.values(metadata.info)){
    let filename = path.basename(info.filename)
    bar.increment(1, {filename: filename})
    for (let tag of info.Tags){
      let tag_linkname = path.join("all_tags", tag, filename)
      await makeRelativeSymlink(info.filename, tag_linkname, how)

    }
    for (let artist of info.Artist){
      let artist_linkname = path.join("all_artists", artist, filename)
      await makeRelativeSymlink(info.filename, artist_linkname, how)
    }
    let date_name = `${info.Released}_${filename}`
    let date_linkname = path.join("all_by_date", date_name)
    await makeRelativeSymlink(info.filename, date_linkname, how)
    let year_linkname = path.join("all_by_year", String(info.Year), date_name)
    await makeRelativeSymlink(info.filename, year_linkname, how)
  }
  bar.stop()
}
const commandOptions = {
  update: { type: 'boolean', short: 'u', default: false }, //use existing metadata only
  replace: { type: 'boolean', short: 'r', default: false } //replace any existing metadata/symlinks
}

async function main(){
  let { values: options, positionals: dirs } = parseArgs({options: commandOptions, allowPositionals: true})
  if(dirs.length == 0 && !options.update){
    console.log(`Usage ${process.argv[1]} [-u|--update] [-r|--replace] directories`)
    process.exit(1)
  }
  //I'll make a real argument parser if I need to
  if(options.update){
    let metadata = await readMetadataFile()
    console.log("Creating symlinks")
    await makeSymlinks(metadata, options.replace)
  } else {
    let metadata = await parseMetadata(dirs, options.replace)
    console.log("Writing metadata file")
    await writeFile(metadataFilename, JSON.stringify(metadata))
    console.log("Creating symlinks")
    await makeSymlinks(metadata, options.replace)
  }
}
// Pretty sure I don't need the try/catch, not sure about the await on main though
try {
  await main()
} catch(err){
  console.error(err)
  throw err
}
