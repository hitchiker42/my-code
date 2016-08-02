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
    say("      -c|--chars chars: specify the possible characters to use when generating a string");
    say("        should be a regular expression that matches 1 character (default \"\\w\"");
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
my $type = "int";
my $sep = ' ';
my $header;
my $chars = qr/[a-zA-Z0-9_]/;
GetOptions("length|l=i" => \$len,
           "maximum|M=f" => \$max,
           "minimum|m=f" => \$min,
           "type|t=s" => \$type,
           "seperator|s=s" => \$sep,
           "header:s" => \$header,
           "chars|c=s" => \$chars,
           "help|h" => \&usage);
my @types = ("integer","float","string");
my $type = unique_prefix($type,@types);
my $arr = [];
# requires a digit before the '.', but not after
my $float_regexp = qr/^[+-]?\d+\.\d*(?:e[+-]?\d+)?\z/i;
if(defined($header)){
    if($header eq ""){
        say("$len");
    } else {
        say(unbackslash("$header"));
    }
}
local $" = unbackslash($sep);
# For arrays larger than 10,000,000 (1,000,000 for strings) print them in parts
# to avoid running out of memory
if(($type eq "float") ||
   ((("$min" =~ m/$float_regexp/) || ("$max" =~ m/$float_regexp/)) && (!defined($type)))){
    while($len > 0){
        $arr = [ map {rand($max-$min)+$min} (1..min($len,10000000)) ];
        say("@$arr");
        $len -= 10000000;
    }
} elsif($type eq "string") {
# For strings min and max are for the strings length
# The defaults are changed to 4-20
    if($max == 2**32){
        $max = 20;
    }
    if($min == 0){
        $min = min($max,4); # make sure min <= max
    }
    $max = min($max, 1000); #limit strings to 1000 characters
    my @chars = grep {m/$chars/} (map {chr($_)} (0..255));
    #rand(@chars) is equivalent to int(rand(scalar(@chars)))
    while($len > 0){
        $arr = [ map {join('',map {$chars[rand(@chars)]}
                           (1..int(rand($max-$min)+$min)))} (1..min($len,1000000)) ];
        say("@$arr");
        $len -= 1000000;
    }
} else {
    while($len > 0){
        $arr = [ map {int(rand($max-$min)+$min)} (1..min($len,10000000)) ];
        say("@$arr");
        $len -= 10000000;
    }

}
