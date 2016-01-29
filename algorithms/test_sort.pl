#!/usr/bin/env perl
use v5.18.0;
use Math::Random::MT::Auto qw(rand irand);
use List::MoreUtils qw(pairwise any zip natatime);
use Getopt::Long qw(:config gnu_getopt);
use Smart::Comments;
no warnings "experimental";
sub rand_arr {
    my $len = shift || 100;
    my $max = shift || (2**16);
    my $arr = [];
    #irand is faster, but it's difficult to use to generate
    #numbers in a range evenly
    $arr = [ map {int(rand($max))} (1..$len) ];
    return $arr;
}
sub compare_arr {
    my ($A,$B) = @_;
    if(scalar(@$A) != scalar(@$B)){
        return 0;
    }
    my $idx = 0;
    my $iter = natatime(2,zip(@$A,@$B));
    while(my ($x,$y) = $iter->()){
        if($x == $y){
            $idx++;
        } else {
            return $idx;
        }
    }
    say("arrays are equal");
    return;
}
my $len = 100;
my $max = 2**16;
my $program = "";
GetOptions ("length|l=i" => \$len,
            "maximum|m=i" => \$max);
my $program = shift;
if(!$program){
    say("No sorting program given");
    exit(1);
}
my $program_args = [@ARGV];
my $rand_arr = rand_arr($len,$max);
say("Random Array: @$rand_arr");
my $control = [sort(@$rand_arr)];
say("Calling program as \"$program @$program_args \"@$rand_arr\"\"");
my $output = qx($program @$program_args "@$rand_arr");
say("Program Returned: $output");
my $test = [split(' ',$output)];
my $mismatch = compare_arr($control, $test);
if(!defined($mismatch)){
    say("Program properly sorted the array");
#    say("Program Returned: @$test");
} else {
    say("Failed to sort array, 1st error at element $mismatch");
    say("Sorted Array: @$control");
    say("Program Returned: @$test");
}

    
