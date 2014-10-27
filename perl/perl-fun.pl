use 5.16.0;
sub add {
    my $ans = shift(@_);
    while (@_) {
	$ans += shift(@_);
    }
    return $ans;
}
say add(@ARGV);
