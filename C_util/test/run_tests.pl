#!/usr/bin/env perl
use v5.18.0;
use File::Temp qw(tempfile);
no warnings "experimental";
my @tests = ("test_hash","test_sort","test_rbtree","test_btree");
sub rand_strings {
    my ($outfile, $len,$min,$max) = @_;
    $len = $len || 100;
    $min = $min || 4;
    $max = $max || 20;
    my @chars = grep {m/$chars/} (map {chr($_)} (0..255));
    #rand(@chars) is equivalent to int(rand(scalar(@chars)))
    while($len > 0){
        $arr = [ map {join('',map {$chars[rand(@chars)]}
                           (1..int(rand($max-$min)+$min)))} 
                 (1..min($len,1000000)) ];
        say($outfile "@$arr");
        $len -= 1000000;
    }
}
sub rand_floats {
    my ($outfile, $len, $min, $max) = @_;
    $len = $len || 100;
    $min = $min || 0;
    $max = $max || 2**32;
    while($len > 0){
        $arr = [ map {rand($max-$min)+$min} (1..min($len,10000000)) ];
        say($outfile "@$arr");
        $len -= 10000000;
    }
}
sub rand_ints {
    my ($outfile, $len, $min, $max) = @_;
    $len = $len || 100;
    $min = $min || 0;
    $max = $max || 2**32;
    while($len > 0){
        $arr = [ map {int(rand($max-$min)+$min)} (1..min($len,10000000)) ];
        say("@$arr");
        $len -= 10000000;
    }
}

