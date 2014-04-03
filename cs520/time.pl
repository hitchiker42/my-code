#!/usr/bin/env perl
#average the time it takes to run a command over 100 iterations
use v5.10.0;
my $real=0;
my $user=0;
my $sys=0;
my $count=100;
while($count--){
  my $raw_output=qx"bash -c 'time ./prog5 prog5_files_all &>/dev/null' 2>&1";
  print($count);
  my @output=split('\n',$raw_output);
  shift(@output);#get rid of leading newline
  for(@output){s/[a-z]*\s*0m(0\.[0-9]{3})s/\1/}
  $real+=$output[0];
  $user+=$output[1];
  $sys+=$output[2];
}
print("real ",$real/100.0,"\nuser ",$user/100.0,"\nsys ",$sys/100.0,"\n");
  
