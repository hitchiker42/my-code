#!/usr/bin/ruby
#Takes 1-2 arguments, location of game to play and optional location of keymap file, defaults to loading a qwerty keymap
if ARGV[1]==TRUE
  `xmodmap '#{ARGV[1]}'`
else
  `xmodmap /home/tucker/xmodmap/qwerty_simple`
end
`LANG=ja_JP.UTF-8 Wine #{ARGV[0]}`;`xmodmap /home/tucker/xmodmap/dvorak_simple`
puts "Normal Termination"
