sub dotimes {
    my $count=shift(@_);
    while ($count > 0) {
	system(@_);
	$count-=1;
    }
}
dotimes(@ARGV);
       
