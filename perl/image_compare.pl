#!/usr/bin/env perl
#Find duplicate images in a directory by comparing imgages based on visual
#similarity rather than simply testing binary equality 
use v5.18.0;
no warnings 'experimental';
use Digest::MD5 qw(md5 md5_hex);
use File::stat;
sub file_size {
    my $fh = shift;
    my $stat = stat($fh);
    return $stat->size;
}
sub basename {
    my $filename = shift;
    $filename =~ m|^.*/([^/]*)$|;#fails when there is a trailing /
    return $1;
}
        
#test if two images are identical, this might be overkill
sub md5_compare {
    my ($img1_filename,$img2_filename) = @_;
    open(my $img1, "<", $img1_filename);
    open(my $img2, "<", $img2_filename);
    my $img1_size = file_size($img1);
    my $img2_size = file_size($img2);
    #99% of the time non-indentical files will exit here
    if($img1_size != $img2_size){
        return 0;
    }
    #the odds that two different images will have the same size is pretty
    #low, but not impossible
    unless(sysread($img1, my $img1_data, $img1_size) &&
           sysread($img2, my $img2_data, $img2_size)){
        die("Error reading $img1_filename or $img2_filename");
    }
    close($img1);
    close($img2);
    return (md5($img1_data) == md5($img2_data));
}
sub phash_compare {
    my ($img1_filname, $img2_filename) = @_;
    my $phash_result =
        `compare -metric PHASH $img1_filname $img2_filename /dev/null`;
#a lower value means a better match, I read somewhere that 21 is a good
#match threshold, but I only want to catch near-exact matches so I'm using 1
    return ($phash_result <= 1 ? 1 : 0);
}

# Finding duplicate images in a directory will take O(N^2) time, where
# N is the number of files, comparing images via phash is faster and more
# robust than some other methods, but it's still quite slow, so ultimately
# this script will take a long time to run
sub find_duplicates {
    my $files = shift; #array containing a list of filenames
    my $num_files = scalar(@{$files});
    my $dups = [];
    my ($file1,$file2);
    #we need to use a for rather than a foreach so we can save time
    #by only comparing a,b and not b,a
    for(my $i = 0;$i < $num_files;$i++){
        $file1 = $files->[$i];
        for(my $j = $i;$j < $num_files;$j++){
            $file2 = $files->[$j];
            if(md5_compare($file1, $file2) || phash_compare($file1, $file2)){
                push($dups,[$file1,$file2]);
            }
        }
    }
    #If file1, file2 and file3 are all identical
    #only [file1,file2] [file1,file3] and [file2,file3] will be in dups
    return $dups;
}

sub delete_dups {
    my $dups = shift;
    my $interactive = shift;
    my $deleted = [];
    if($interactive){
        #prompt to decide what to delete and what to keep
    } else {
        #this might not work because of references
        for my $match ($dups) {
            if($match->[0] ~~ $dups){#smartmatch, test array membership
                next;
            }
            push($deleted, $match->[1]);
            #this should probably be safer than this
            `rm $match->[1]`;
        }
    }
}

    
