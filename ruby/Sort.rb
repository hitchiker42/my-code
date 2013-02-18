#!/usr/bin/ruby
#Made by Tuckor DiNapoli, Last Edited 10/6/2012
#Program to move all files in a directory matching a given condition to a given directory
require 'scanf'
ls=`ls`.to_s.lines.to_a
ext=ARGV[0]#argument 1 is the condition (usually a file extention) to sort
dir=ARGV[1]#argument 2 is the directory to move files to
if `find #{dir} -type d`==""#if dir does not exist make it, name it "dir"-dir if the name is taken 
  if `find #{dir}`==""
    `mkdir #{dir}`
  else
    `mkdir #{dir}-dir`;dir="#{dir}-dir"
end
end
find=Regexp.new(ext)#Find all files with ext and put ithen into array lsf
lsf=Array.new
ls.each{|i|
  if find=~i
    lsf << i end}
puts lsf
puts 
puts "Are you sure you wish to move these files"
ans=scanf("%c")[0]#show files and confirm with the user than move all entries of lsf to dir
if ans=='y'
  lsf.each{|i| i.delete!("\n");`mv '#{i}' '#{dir}'`}
  puts "Files Successfully Moved"
else
  puts "Files will not be moved"
end
puts "Goodbye!"
