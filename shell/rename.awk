BEGIN {RS="\0";ORS="\n"};
{a=gensub(/\s+-\s+/,"-","g",$0)
  b=gensub(/[~!@#$%\^&*+?=\\'"`~]+/,"","g",a)
  #this is temporary,I'm not sure what to do with delimiters yet
  #c=gensub(/\s*\(\s*(\S*)\s*\)\s*/,"\[\\1\]","g",b)
  c=gensub(/\s*\(\s*/,"[","g",b)
  d=gensub(/\s*\)\s*/,"]","g",c)
  #           ws,(,ws,1=!ws,ws,),ws1,global,b replace parentheses w/brackets'
  #d=gensub(/\s*<\s*(.*)\s*>\s*/,"\[\\1\]","g",b)
  e=gensub(/\s+/,"_","g",d)
  print e}
