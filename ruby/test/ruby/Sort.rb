#!/usr/bin/ruby 
require 'scanf'
ls=`ls`.to_s.lines.to_a
ext=ARGV[0]
dir=ARGV[1]
if `find #{dir} -type d`==""
  if `find #{dir}`==""
    `mkdir #{dir}`
  else
    `mkdir #{dir}-dir`;dir="#{dir}-dir"
end
end
find=Regexp.new(ext)
lsf=Array.new
ls.each{|i|
  if find=~i
    lsf << i end}
puts lsf
puts 
puts "Are you sure you wish to move these files"
ans=scanf("%c")[0]
if ans=='y'
  lsf.each{|i| i.delete!("\n");`mv '#{i}' '#{dir}'`}
  puts "Files Successfully Moved"
else
  puts "Files will not be moved"
end
puts "Goodbye!"
