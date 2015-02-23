package Util;
use v5.18;
sub mapcat (&@) {
    my $fun = shift;
    my @retval;
    for my $val (@_) {
        @retval = $fun->($val,@retval);
    }
    return \@retval;
}

