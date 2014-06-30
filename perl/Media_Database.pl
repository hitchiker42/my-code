use v5.18.0;
use DBI;
our $dbh;
sub uniq{
    # Eliminates redundant values from sorted list of values input.
    my $prev;
    my @out;
    foreach my $val (@_){
        if($prev && ($prev eq $val)){
            next;
        }
        $prev = $val;
        push(@out, $val);
    }
    return @out;
}
sub find_authors {
    my $dir = shift;
    if(!$dir){
        $dir='.';
    }
    my @filenames = uniq(sort(`find -maxdepth 1 -type f -printf "%f\n"`));
    map {m/[[{](\w+)([- ]\w+)[]}]/ ? s/ /_/g : ()} @filenames;
    #Need to do something with this;
}
sub DBI_statement {
    my ($dbh,$statement,@argss) = @_;
    $sth = $dbh->prepare($statement);
    $sth->execute{ @args };
}
#Option parsing goes here
my $db_filename = shift;
$dbh = DBI->connect("dbi:SQLite:dbname=$db_filename","","");
