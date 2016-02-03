#!/usr/bin/env perl
use v5.18.0;
use Getopt::Long qw(:config gnu_getopt);
no warnings "experimental";
eval("use Math::Random::MT::Auto qw(rand)");
sub usage {
    say("Usage: rand_arr [-l|length (100)] [-M|--max (2^32)] [-m|--min (0)]");
    say("    Optionally -i|--int / -f|--float can be given to specify the type");
    say("      of the output, by default they are integers");
}
my $len = 100;
my $min = 0;
my $max = 2**32;
my $int = 0;
my $float = 0;
my $sep = ' ';
GetOptions("length|l=i" => \$len,
           "maximum|M=f" => \$max,
           "minimum|m=f" => \$min,
           "float|f" => \$float,
           "integer|i" => \$int,
           "seperator|s=s" => \$sep);
my $arr = [];
if($float && !$int){
    $arr = [ map {rand($max-$min)+$min} (1..$len) ];
} else {
    $arr = [ map {int(rand($max-$min)+$min)} (1..$len) ];
}
local $" = $sep;
say("@$arr");
