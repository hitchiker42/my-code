#!/bin/bash
#for now, read filenames from dir and sub _ for all whitespace and print results to a file
[[ -z $1 ]] && echo "takes a pathname as an arguement" && exit
directory=$1
find "$directory" -maxdepth 1 -mindepth 1 -print0 | xargs -0 basename -za >temp.ls
cat temp.ls | awk -f rename.awk | tee temp.names
rm temp.ls
