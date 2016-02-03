#!/usr/bin/env perl
use v5.18.0;
use Getopt::Long qw(:config gnu_getopt);
use String::Escape qw(unbackslash);
no warnings "experimental";
eval("use Math::Random::MT::Auto qw(rand)");
sub usage {
    say("Usage: rand_arr options [-l|length (100)] [-M|--max (2^32)] [-m|--min (0)]");
    say("    Options:");
    say("      -i|--int / -f|--float specify the type of numbers (type of min/max by default)");
    say("      -s|--seperator str: specify the delimiter printed between elements");
    say("      --header[=str]: print a header before the array, if no value is given");
    say("        the length of the array will be printed");
    exit(0);
}
my $len = 100;
my $min = 0;
my $max = 2**32;
my $int = 0;
my $float = 0;
my $sep = ' ';
my $header;
GetOptions("length|l=i" => \$len,
           "maximum|M=f" => \$max,
           "minimum|m=f" => \$min,
           "float|f" => \$float,
           "integer|i" => \$int,
           "seperator|s=s" => \$sep,
           "header:s" => \$header,
           "help|h" => \&usage);
my $arr = [];
my $float_regexp = qr/^-?(?:\d+\.?|\.\d)\d*\z/;
if($float || (($min =~ $float_regexp || $max =~ $float_regexp) && !$int)){
    $arr = [ map {rand($max-$min)+$min} (1..$len) ];
} else {
    $arr = [ map {int(rand($max-$min)+$min)} (1..$len) ];
}
if(defined($header)){
    if($header eq ""){
        say("$len");
    } else {
        say(unbackslash("$header"));
    }
}
local $" = unbackslash($sep);
say("@$arr");
