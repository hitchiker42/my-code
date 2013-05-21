#!/bin/bash
#for now, read filenames from dir and sub _ for all whitespace
#then print results to a file
[[ -z $1 ]] && echo "takes a pathname as an arguement" && exit
directory=$1
find "$directory" -maxdepth 1 -mindepth 1 -print0 |\
 xargs -0 basename -za >temp.ls

cat temp.ls | awk -f /home/tucker/Repo/my-code/shell/rename.awk >temp.names

cat temp.ls |\
 sed s/"[[:space:][:cntrl:][:punct:]]"/'\\&'/g | tr '\000' '\n' >temp.paste

rm temp.ls
