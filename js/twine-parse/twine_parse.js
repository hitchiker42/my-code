#!/usr/bin/env node
/*
  Takes a twine (sugarcube) html file and decomposes it into passages, scripts and stylesheets
  Any user scripts or stylesheets are written to their original filenames
  Passages are written to a file in the form:
  Name
  \f
  contents
*/
/*
  Twine html structure, excluding meta nodes
  <html>
  <head>
  <title> title </title>
  <script id="script-libraries">
  Js library code
  </script>
  (<style id="style-.*"> css </style>)+
  </head>
  <body>
  <tw-storydata name="title" start-node="start" ...>
  <style id="twine-user-stylesheet">
  / * twine-user-stylesheel #N: "title.css" * / //only if from an external file, one for each file
  style content
  </style>
  <script id="twine-user-script>
  / * twine-user-script #N: "title.js" * / //only if from an external file, one for each file
  script code
  </script>
  (<tw-passagedata pid="N" name="name"> contents </tw-passagedata>)*
  </tw-storydata>
  <script id="script-sugarcube"> sugarcube js code </script>
  </body>
*/
const cheerio = require("cheerio")
const fs = require("fs")

function parse_scripts_and_styles(text, scripts = true){
  if(!text){ return }
  let re = (scripts ? /\/\* twine-user-script #(\d+): "(.*\.js)" \*\//g
                    : /\/\* twine-user-stylesheet #(\d+): "(.*\.css)" \*\//g)
  let handle_err = (err) => {if(err) throw err;}
  if(!text.startsWith("/* twine-user-")){
    fs.writeFile((scripts ? "script.js" : "style.css"), text, handle_err)
    return
  }
  let match = re.exec(text)
  let start = re.lastIndex
  do {
    let filename = match[2]
    match = re.exec(text)
    let end = match ? match.index : text.length
    //the +1 is to avoid a leading newline on each file
    fs.writeFile(filename, text.substring(start+1, end), handle_err)
  } while((start = re.lastIndex) !== 0) //set start & check for end at same time
}
let parse_scripts = (text) => parse_scripts_and_styles(text, true)
let parse_styles = (text) => parse_scripts_and_styles(text, false)
function parse_text(text, out){
  //Cheerio automatically takes care of escaping stuff, so we don't
  //really need to do anything. With regards to html escapes, the code below
  //is kept for posterity. We do however unescape newlines, since this is meant
  //to make the output more readable.
  let start = 0, index = 0;
  while((index = text.indexOf("\\n", start)) > 0){
    out.write(text.substring(start, index));
    out.write("\n");
    start = index;
  }
  out.write(text.substring(start, text.length));
/*  let regexp = /&(amp|lt|gt|quot|apos|#39);/g
  let replacements = {"amp": "&", "lt": "<",
                      "gt": ">", "quot": "\""}
//                      "apos": "'",
//                      "#39": "'"}
  let next = 0
  let match = undefined
  while((match = regexp.exec(text))){
    out.write(text.substring(next, match.index))
    out.write(replacements[match[1]])
    next = regexp.lastIndex
  }
  out.write(text.substring(next, text.length))*/
}
function process_twine($, sort){
  //we really only need the storydata, so this simplifies later selections
  let scripts = $("#twine-user-script").html()
  let styles = $("#twine-user-stylesheet").html()
  let passages = $("tw-passagedata")
  let name = "name"
  //console.log("parsing scripts")
  parse_scripts(scripts)
  //console.log("parsing stylesheets")
  parse_styles(styles)
  let out = fs.createWriteStream("passages.txt")
  if(passages.length == 0){
    //Not the usual sugarcube format, try another
    passages = $("body div[tiddler]")
    name = "tiddler"
  }
  if(sort){
    passages.sort((a,b) => ((a.getAttribute(name) < b.getAttribute<name) ? -1 : 1))
  }
  for(let passage of passages){
    passage = $(passage)
    let title = passage.attr(name)
    out.write(title + "\n\f")
    parse_text(passage.text(), out)
    out.write("\n\n\n")
  }
  out.end()
}
function main(){
  let args = process.argv.slice(2)
  if(args.length == 0){
    console.log("Usage %s file.html [output_dir] ", process.argv[1])
    return;
  }
  let twine_file = args[0]
  let explicit_dir = args.length >= 2
  let output_dir = explicit_dir ? args[1] : args[0].replace(/(.*)\..*/, "$1")
  if(output_dir == twine_file){//sanity check
    output_dir += "_d"
  }
  let $ = cheerio.load(fs.readFileSync(twine_file), {decodeEntities: false})
  try {
    fs.mkdirSync(output_dir)
  } catch(err){
    if(err.code == "EEXIST"){
      if(!explicit_dir){
        console.log("Output directory %s exists, provide it as an explicit argument to use it anyway", output_dir)
        return
      }
    } else {
      throw err
    }
  }
  process.chdir(output_dir)
  process_twine($)
}
if(require.main === module){
  main()
}
