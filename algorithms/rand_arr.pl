#!/usr/bin/env perl
use v5.18.0;
use Math::Random::MT::Auto qw(rand irand);
no warnings "experimental";
my $len = shift || 100;
my $max = shift || (2**16);
my $arr = [];
#irand is faster, but it's difficult to use to generate
#numbers in a range evenly
$arr = [ map {int(rand($max))} (1..$len) ];
say("@$arr");
