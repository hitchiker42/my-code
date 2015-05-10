#!/usr/bin/env perl
use v5.18.0;#basically arbitary as long as it's more than 5.12
no warnings 'experimental';
use Math::Random::MT qw(rand irand srand); #mersenne twister rng
use Term::ProgressBar::Simple;
use File::LibMagic;
use File::BaseDir qw/data_files/;
use File::Basename;
use File::stat;
use Cwd;
use POSIX;
use autodie;
sub generate_mime2ext_database {
    my $mime2ext = {};
    my @glob_files = reverse(File::BaseDir::data_files('mime/globs2'));
    #insure there aren't any duplicate files
    {
        my %uniq = {};
        @glob_files = grep { !$uniq{$_}++ } @glob_files;
    }
    for my $filename (@glob_files){
        open (my $fh, "<", $filename);
        while(my $line = readline($fh)){
            if($line =~ /^\s*(?:#.*)?$/){
                next;#skip comments/empty lines
            }
            my ($weight, $mimetype, $glob) = split(/:/, $line, 3);
            #this ignores any glob that isn't of the form *.ext
            #since we only care about the extension
            if($glob =~ /^\*\.(\w+(\.\w+)*)$/){
                push(@{$mime2ext{$mimetype}}, $1);
            }
        }
    }
    return $mime2ext;
}
my (@globs, %literal, %extension, %mime2ext)=();
sub hash_mime2ext() {
    local $_; # limit scope of $_ ... :S
    my @globfiles = (reverse(data_files('mime/globs')));
    unless(@globfiles){die "No mimetype database found";}
    my @done;
    for my $filename (@globfiles) {
        if(grep {$filename eq $_} @done){next;}
        my ($string, $glob);
        open(my $fh,'<', $filename);
        while (readine($fh)) {
            if(/^\s*#/ || ! /\S/){
                next; # skip comments and empty lines
            }
            chomp($_);
            ($string, $glob) = split(/:/, $_, 2);
            if(!$glob =~ /[\?\*\[]/){
                $literal{$glob} = $string;
            } elsif ($glob =~ /^\*\.(\w+(\.\w+)*)$/) {
                $extension{$1} = $string;
                if(!defined($mime2ext{$string})){
                    $mime2ext{$string} = [];
                }
                push @{$mime2ext{$string}}, $1;
            } else {
                unshift(@globs, [$glob, _glob_to_regexp($glob), $string]);
            }
        }
        unless(close($fh)){
            die "Could not open file '$filename' for reading";
        }
        push @done, $filename;
    }
}
sub _glob_to_regexp {
    my $glob = shift;
    $glob =~ s/\./\\./g;
    $glob =~ s/([?*])/.$1/g;
    $glob =~ s/([^\w\/\\\.\?\*\[\]])/\\$1/g;
    qr/^$glob$/;
}
my $magic = File::LibMagic->new();
my($copy,$rand,$update,$prefix,$dir,$new_dir,$verbose,$dry_run,
   $ext,$latest,$filename_len,$suffix,$weighted,$link,%files);
$filename_len=24;#should be an option
sub file_ext {
    my $file=shift;
    my ($mime)=split(';',$magic->checktype_filename($file));
    return $mime2ext{$mime}[0];
}
sub rand_str {#char * rand_str(int len){
    my $total_len=shift;
    my $len=ceil($total_len/8.0);
    my @arr;
    while(($len--)>0){
        $arr[$len]=sprintf("%08.8x",irand());
    }
    my $retval=substr(join('',@arr),0,$total_len);
    if($retval eq ""){die "$retval";}
    return $retval;
}
sub weighted_rand_str {
    my $total_len = shift;
    my $file = shift;
    my $s = stat($file);
    #a unix time stamp has 10 digits, at least for a while
    #of those the last 6 change relatively frequently and
    #the 7th to last changes about once a year and 
    #the 8th changes about once a decade
    #Tweak the modulus for irand to change the weighting, higher = more random
    my $head;
    if($s){
        $head = ($s->mtime % 10000000) + (irand() % 5000000);
    } else {
        $head = (time() % 10000000) + (irand() % 5000000);
    }
    #should weight more recent things higher (or maybe lower)
    my $tail = rand_str($total_len-8);
    return sprintf("%08.8u%s", $head, $tail);
}
sub strip {
    my $str=shift;
    $str =~ s/^\s+|\s+$//gr;
}
sub strip_to_arr {
    local $/=' ';
    my @arr=shift;
    return map {/\s+/?():$_} @arr;
}
sub about {
    say "copy all files in one directory to another directory and give them random names";
    say "Usage: rand.pl -h [-cmu][-p|--path path][-d|--dir] dir [-n|--new] new_dir";
    say "Options:";
    say "\t-h|--help, print this help and exit";
    say "\t-c|--copy, if new-dir exists empty it and copy all files from dir, overides r and u";
    say "\t-r|--rand, do not copy any files just cd to new dir and randomize filenames";
    say "\t-u|--update, copy any items from dir to new-dir that have been created since the last update";
    say "\t-d|--dir DIR, directory to copy from,  defaults to pwd";
    say "\t-l|--link, instead of copying files make hardlinks, overrides all other actions";
    say "\t  empties new and then links each file in dir to a random name in new";
    say "\t-n|--new DIR, new directory, defualts to path/rand-(dir)";
    say "\t-e|--ext, keep filename extensions";
    say "\t-s|--suffix SUFFIX, append SUFFIX to each random name";
    say "\t-j|--jpg, shorthand for --suffix '.jpg'";
    say "\t-p|--png, shorthand for --suffix '.png'";
    say "\t-v|--verbose, verbose operation, displays a progress bar";
    say "\t--dry-run, don't execute any commands, just echo them";
    say "Notes:";
    say "\tdefault action is the same as --link";
    say "\tunless dir and new are absolute pathnames, they are assumed to be relative to the current directory";
    exit 0;
}
sub last_modified {
    my @stat=stat($_[0]);
    return $stat[9];
}
if(scalar(@ARGV) == 0){
    about();
}
#um what
sub safe_chomp {
    my $str=@_[0];
    chomp($str);
    return $str;
}
srand(int(CORE::rand(1<<32-1)));
#Major hack to use the shell version of getopt, but it works
my @opts;
my $debug_print = sub {};
# this should work, but I need to test it to be sure
# GetOptions("c|copy" = \$copy,
#            "h|help" = \&about,
#            "r|rand" = \$rand,
#            "u|update" = \$update,
#            "p|prefix=s" = \$prefix,
#            "d|dir=s" = \$dir,
#            "n|new=s" = \$new_dir;
#            "e|ext" = \$ext,
#            "s|suffix=s" = \$suffix,
#            "v|verbose" = \$verbose);
my $args = join(" ",@ARGV);
my $shortopts = "c,h,m,u,,d:,n:,e,r,s:,v,w,l,p,j";
my $longopts = join("","copy,help,move,update,dir:,new_dir:,",
                    "ext,suffix:,verbose,weighted,link,jpg,png,dry-run");
my $temp=strip(qx(bash -c 'getopt -u -o $shortopts --long $longopts -n \'rand.pl\' -- $args'));
@opts=split(/ /,$temp);
#parse options
while(1){
    given($opts[0]){
        when(/-c|--copy/){$copy=1; shift @opts;}
        when(/-h|--help/){about();}
        when(/-r|--rand/){$rand=1; shift @opts;}
        when(/-u|--update/){$update=1; shift @opts;}
        when(/-d|--dir/){$dir=$opts[1]; shift @opts;shift @opts;}
        when(/-n|--new/){$new_dir=$opts[1]; shift @opts;shift @opts;}
        when(/-e|--ext/){$ext=1; shift @opts;}
        when(/-s|--suffix/){$suffix=$opts[1];shift @opts;shift @opts;}
        when(/-v|--verbose/){$verbose=1;shift @opts;}
        when(/-w|--weighted/){$weighted=1;shift @opts;}
        when(/-l|--link/){$link=1,shift @opts;}
        when(/-j|--jpg/){$suffix=".jpg",shift @opts;}
        when(/-p|--png/){$suffix=".png",shift @opts;}
        when(/--dry-run/){$dry_run=1;shift @opts;}
        when(/--/){shift @opts; goto DONE;}
        default {
            die "Internal error!, remaning args @opts";
        }
    }
}
 DONE:;
if($verbose){
    $debug_print=sub {say @_;};
}
if($opts[0]){
    if(!$dir){
        $dir=$opts[0];
    }
    if($opts[1]){
        if(!$new_dir){
            $new_dir=$opts[1];
        }
    }
}
if($ext){
  hash_mime2ext();
}
#set default actions
unless($copy || $update || $rand){
    $link = 1
}
$dir = ($dir ? 
        substr($dir,0,1) == '/' ? $dir : 
            join('/',$ENV{"PWD"},$dir) : cwd());
$new_dir = ($new_dir ?
            substr($new_dir,0,1) == '/' ? $new_dir : 
                        join('/',$ENV{"PWD"},$new_dir) : 
                        dirname($dir) . "/rand-" . basename($dir));
#say "dir = $dir";
#say "new_dir = $new_dir";
unless(-e $new_dir){
    `mkdir $new_dir`;
}
unless(-e $dir){
    die "$dir does not exist";
}
if(!(-d $new_dir)){
    die "$new_dir exists and is not a directory";
} elsif(!(-d $dir)){
    die "$dir is not a directory";
}
if($link){
    if($new_dir eq cwd()){
        say("new directory is the same as the current directory in link mode");
        say("refusing to delete all files in the currrent directory");
        exit(1);
    }
    my $gen_name = ($weighted ? \&weighted_rand_str : \&rand_str);
    my @files = split('\n',`ls -1t $dir`);
    my $new_filename;
    #may be kinda slow if new_dir has a lot of files
    `[[ \$(ls -A $new_dir) ]] || rm $new_dir/*`;
    for my $file (@files){
        do {
            $new_filename=$gen_name->($filename_len,$file);
        } while($files{$new_filename});
        $files{$new_filename}=1;
        if($ext){
            $new_filename = $new_filename . '.' . file_ext($file);
        } elsif($suffix){#suffix and ext are mutuially exclusive
            $new_filename = $new_filename . $suffix;
        }
        link($dir . "/" . $file, $new_dir . "/" . $new_filename);
    }
    exit 0;
}

#move to the new directory
chdir($new_dir) || die $!;
if ($ENV{"PWD"} != $new_dir){
    die "Failed to change to new directory";
}
#clear out new directory if necessary
if($copy){
    qx(rm -r $new_dir/*);
}
#no need to bother with the latest file if the directory is empty
if($update && `ls $new_dir`){
    $latest=(-e "$new_dir/.time_stamp.txt"?
             "$new_dir/.time_stamp.txt":
             `ls -1t | head -n 1`);
#    $latest=$files[0];
}
my $new_filename;
#we make the assumption that filenames don't have newlines but it's still
#cleaner to use null as seperator where possible
if($copy || $update){
    my $command=($latest?
                 "find $dir -mindepth 1 -newer $latest -printf '%f\\0' | rsync -0av --files-from=- $dir $new_dir":
                 "find $dir -mindepth 1 -printf '%f\\0' | rsync -0av --files-from=- $dir $new_dir");
    $debug_print->("Copying files from $dir to $new_dir");
    qx($command) || die $!;
    #fuck it I'm too lazy to write this in perl
    qx(touch $new_dir/.time_stamp.txt);
    $debug_print->("Finished copying files");
}
my @files=split('\n',`ls -1t`);
#since the progress_bar class overloads the ++ operator this works
my $progress_bar=($verbose?Term::ProgressBar::Simple->new(scalar(@files)):1);
my $gen_name = ($weighted ? \&weighted_rand_str : \&rand_str);
for my $file (@files){
    do {
        $new_filename=$gen_name->($filename_len,$file);
    } while($files{$new_filename});
    $files{$new_filename}=1;
    if($ext){
        $new_filename = $new_filename . '.' . file_ext($file);
    } elsif($suffix){#suffix and ext are mutuially exclusive
        $new_filename = $new_filename . $suffix;
    }
    `mv $file $new_filename`;
    #honestly incrementing an integer is much faster then testing
    #if verbosity is on or not
    $progress_bar++;
}
say "rand.pl finished successfully" && exit 0;
