#!/usr/bin/env perl
use v5.18.0;
no warnings "experimental";
use autodie;
use List::Util qw(shuffle);
my $program_args = "8";
my $outfile = @ARGV[0] || "output${program_args}.csv";
my $num_runs = 8;
my $cflags = "-march=native -std=c99 -fomit-frame-pointer";
my $clang_flags = "$cflags -fopenmp=libomp";
my $data = {gcc => {"0" => [], "1" => [], "2" => [], "3" => []},
            icc => {"0" => [], "1" => [], "2" => [], "3" => []},
            clang => {"0" => [], "1" => [], "2" => [], "3" => []}};
open(my $fh, ">", $outfile);
print($fh "compiler, opt, time, user, sys, user_sys\n");
# Compile everything
for my $compiler (keys($data)){
    for my $opt_level (keys($data->{$compiler})){
        my $name = "$compiler$opt_level";
        if($compiler eq "clang"){
            `$compiler -o $name -O$opt_level $cflags $clang_flags src.c`;
        } else {
            `$compiler -o $name -O$opt_level $cflags -fopenmp src.c`;
        }
    }
}
# do num_runs runs, selecting the order of compilier and optimization level randomly
for(my $i=0;$i<$num_runs;$i++){
    for my $compiler (shuffle(keys($data))){
        for my $opt_level (shuffle(keys($data->{$compiler}))){
            my $name = "$compiler$opt_level";
            my $runs = $data->{$compiler}{$opt_level};
            #time is a weird command, since it's a shell builtin, we need to
            #enclose it in a block to be able to redirect its output
            my @times = split(' ',`{ export TIMEFORMAT="%R %U %S"; time ./$name $program_args > /dev/null ; } 2>&1`);
            my $user_sys = $times[1] + $times[2];
            print($fh "$compiler, $opt_level, $times[0], $times[1], $times[2], $user_sys\n");
        }
    }
}
