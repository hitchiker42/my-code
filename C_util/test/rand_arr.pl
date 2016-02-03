#!/usr/bin/env perl
use v5.18.0;
use Getopt::Long qw(:config gnu_getopt);
use String::Escape qw(unbackslash);
use List::Util qw(min max);
no warnings "experimental";
#use fancy random number generator, if available
eval("use Math::Random::MT::Auto qw(rand)");
#TODO: Add option to specify range of characters
#TODO: Add option to take random words from a dictionary
sub usage {
    say("Usage: rand_arr options [-l|length (100)] [-M|--max (2^32)] [-m|--min (0)]");
    say("    Options:");
    say("      -t|--type specify the type of array elements, defaults to type of min/max");
    say("        valid values are integer, float, and string");
    say("      -s|--seperator str: specify the delimiter printed between elements");
    say("      --header[=str]: print a header before the array, if no value is given");
    say("        the length of the array will be printed");
    exit(0);
}
sub unique_prefix {
    my $prefix = shift;
    my @matches = grep {index($_, $prefix) == 0} @_;
    if(scalar(@matches) == 1){
        return $matches[0];
    } else {
        return;
    }
}

my $len = 100;
my $min = 0;
my $max = 2**32;
my $type = "";
my $sep = ' ';
my $header;
GetOptions("length|l=i" => \$len,
           "maximum|M=f" => \$max,
           "minimum|m=f" => \$min,
           "type|t=s" => \$type,
           "seperator|s=s" => \$sep,
           "header:s" => \$header,
           "help|h" => \&usage);
my @types = ("integer","float","string");
my $type = unique_prefix($type,@types);
my $arr = [];
my $float_regexp = qr/^[+-]?(?=\.?\d)\d*\.?\d*(?:e[+-]?\d+)?\z/i;
if(($type eq "float") || 
   (($min =~ $float_regexp || $max =~ $float_regexp) && (!defined($type)))){
    $arr = [ map {rand($max-$min)+$min} (1..$len) ];
} elsif($type eq "string") {
    $max = min($max, 1000); #limit strings to 1000 characters
    my @chars = ("1".."9","A".."Z","a".."z");
    #rand(@chars) is equivalent to int(rand(scalar(@chars)))
    $arr = [ map {join('',map {$chars[rand(@chars)]} 
                       (0..int(rand($max-$min)+$min)))} (1..$len) ];
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
