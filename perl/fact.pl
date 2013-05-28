use 5.16.0;
sub fact {
    my $acc=sub{my($n,$fact)=@_;
		if ($n){
		    __SUB__->($n-1,$fact*$n);}
		else {$fact;}
    };
    $acc->(@_,1);
}
my $ans=fact(@ARGV);
say $ans;
