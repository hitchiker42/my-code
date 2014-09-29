#!/usr/bin/env perl
use v5.18.0;#basically arbitary as long as it's more than 5.12
no warnings 'experimental';
use Math::Random::MT qw(rand irand srand); #mersenne twister rng
use Term::ProgressBar::Simple;
use File::LibMagic;
use Cwd;
use POSIX;
use File::BaseDir qw/data_files/;
my (@globs, %literal, %extension, %mime2ext)=();
sub hash_mime2ext() {
    local $_; # limit scope of $_ ... :S
    my @globfiles = (reverse(data_files('mime/globs')));
    unless(@globfiles){die "No mimetype database found";}
    my @done;
    for my $filename (@globfiles) {
        if(grep {$filename eq $_} @done){next;}
        my ($string, $glob);
        open(GLOB,'<', $filename);
        while (<GLOB>) {
            if(/^\s*#/ or ! /\S/){
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
        unless(close GLOB){
            die "Could not open file '$filename' for reading";
        }
        push @done, $filename;
    }
}
sub _glob_to_regexp($) {
    my $glob = shift;
    $glob =~ s/\./\\./g;
    $glob =~ s/([?*])/.$1/g;
    $glob =~ s/([^\w\/\\\.\?\*\[\]])/\\$1/g;
    qr/^$glob$/;
                    }
my $magic = File::LibMagic->new();
my($copy,$rand,$update,$prefix,$dir,$new_dir,
   $ext,$latest,$filename_len,$suffix,%files);
$filename_len=24;#should be an option
sub file_ext($){
    my $file=shift;
    my ($mime)=split(';',$magic->checktype_filename($file));
    return $mime2ext{$mime}[0];
             }
sub rand_str($){#char * rand_str(int len){
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
sub strip($){
    my $str=shift;
    $str =~ s/^\s+|\s+$//gr;
}
sub strip_to_arr($){
    local $/=' ';
    my @arr=shift;
    return map {/\s+/?():$_} @arr;

                 }
sub about() {
    say "copy all files in one directory to another directory and give them random names";
    say "Usage rand.pl -h [-cmu][-p|--path path][-d|--dir] dir [-n|--new] new_dir";
    say "Options:";
    say "\t-h|--help, print this help and exit";
    say "\t-c|--copy, if new-dir exists empty it and copy all files from dir, overides r and u";
    say "\t-r|--rand, do not copy any files just cd to new dir and randomize filenames";
    say "\t-u|--update, copy any items from dir to new-dir that are newer than then newest item in new-dir";
    say "\t-p|--path PATH, base path to use for dir and new_dir, defaults to pwd";
    say "\t-d|--dir DIR, directory to copy from, defaults to pwd";
    say "\t-n|--new DIR, new directory, defualts to path/rand-(dir)";
    say "\t-e|--ext, keep filename extensions";
    say "\t-s|--suffix SUFFIX, append SUFFIX to each random name";
    say "\tdefault action is the same as --update";
    exit 0;
}
sub last_modified($){
    my @stat=stat($_[0]);
    return $stat[9];
                  }
if(scalar(@ARGV) == 0){
    about();
}
sub safe_chomp($){
    my $str=@_[0];
    chomp($str);
    return $str;
               }
srand(int(CORE::rand(1<<32-1)));
hash_mime2ext();
#Major hack to use the shell version of getopt, but it works
my @opts;
# sub getopt($){#char **getopt(char *opts,char *longopts, char *filename);
#     my $args = join(" ",@ARGV);
#     my ($opts,$longopts,$filename)=@_;
#     if(!$filename){
#         $filename=$0;
#     }
#     my $temp=strip(qx(bash -c 'getopt -u -o $opts --long $longopts -n $filename -- $args'));
# }
my $args = join(" ",@ARGV);
my $shortopts = "c,h,m,u,p:,d:,n:,e,r,s:";
my $longopts = "copy,help,move,update,prefix:,dir:,new_dir:,ext,suffix:";
my $temp=strip(qx(bash -c 'getopt -u -o $shortopts --long $longopts -n \'rand.pl\' -- $args'));
@opts=split(/ /,$temp);
#parse options
while(1){
    given($opts[0]){
        when(/-c|--copy/){$copy=1; shift @opts;}
        when(/-h|--help/){about();}
        when(/-r|--rand/){$rand=1; shift @opts;}
        when(/-u|--update/){$update=1; shift @opts;}
        when(/-p|--prefix/){$prefix=$opts[1]; shift @opts;shift @opts;}
        when(/-d|--dir/){$dir=$opts[1]; shift @opts;shift @opts;}
        when(/-n|--new/){$new_dir=$opts[1]; shift @opts;shift @opts;}
        when(/-e|--ext/){$ext=1; shift @opts;}
        when(/-s|--suffix/){$suffix=$opts[1];shift @opts;shift @opts;}
        when(/--/){shift @opts; goto DONE;}
        default {
            die "Internal error!, remaning args @opts";
        }
    }
}
 DONE:;
if(!$prefix){
    $prefix=$ENV{"PWD"};
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

#set default actions
unless($copy || $update || $rand){
  $update=1;
  $rand=1;
}
if(!$dir){
    $dir=cwd();
} else {
    $dir=join('/',$prefix,$dir);
}
if(!$new_dir){
    $new_dir=cwd();
} else {
    $new_dir=join('/',$prefix,$new_dir);
}
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
#move to the new directory
chdir($new_dir) || die $!;
if ($ENV{"PWD"} != $new_dir){
    die "Failed to change to new directory";
}
#clear out new directory if necessary
if($copy){
    `rm -r $new_dir/*`;
}
my @files=split('\n',`ls -1t`);
if($update && $files[0]){
    $latest=$files[0];
}
my $new_filename;
#we make the assumption that filenames don't have newlines but it's still
#cleaner to use null as seperator where possible
if($copy || $update){
    my $command=($latest?
                 "find $dir -mindepth 1 -newer $latest -printf '%f\\0' | rsync -0av --files-from=- $dir $new_dir":
                 "find $dir -mindepth 1 -printf '%f\\0' | rsync -0av --files-from=- $dir $new_dir");
    qx($command) || die $!;
}

my $progress_bar=Term::ProgressBar::Simple->new(scalar(@files));
for my $file (@files){
    do {
        $new_filename=rand_str($filename_len);
    } while($files{$new_filename});
    $files{$new_filename}=1;
    if($ext){
        $new_filename = $new_filename . '.' . file_ext($file);
    } elsif($suffix){#suffix and ext are mutuially exclusive
        $new_filename = $new_filename . $suffix;
    }
    `mv $file $new_filename`;
    $progress_bar++;
}
say "rand.pl finished successfully" && exit 0;
