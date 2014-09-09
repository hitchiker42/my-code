#!/usr/bin/env perl
use v5.18.0;
use autodie;
no warnings 'experimental';
#only checks file extensions, not actual contents
sub get_ext {
    my $filename = shift;
    if ($filename =~ m/.+\.(tar\..+)/ || $filename =~ m/.+\.(.+)/){
        return $1;
    } else {
        return;
    }
}
sub make_dir_name {
    my $filename = shift;
    if ($filename =~ m/(.+)\..+/){
        $filename = $1;
    }
    $filename =~ s/ /_/g;
    $filename =~ s/__+/_/g;
    $filename =~ s/_-_/-/g;
    $filename =~ s/[!?~*]//g;
    return $filename;
}
sub my_system {
    my $exit_val = system(@_);
    if($exit_val == -1){
        exit ($exit_val >> 8);
    } else {
        return $exit_val;
    }
}
my %extract_new_dir = {
    'zip' => "unzip",
    '7z' => "7z x",
    'rar' => "unrar x",
};
my %extract = {
    tar => ("tar","-xf"),
    gz => ("gunzip"),
    bz2 => ("bunzip2"),
    xz => ("unxz"),
    lzma => ("unxz"),
};
if($#ARGV <= 0){
    say("Usage: extract.pl archive-file...");
    say("\tExtract archives according to file type.");
    say("\tDetermines file type soley by file extension.");
    exit 0;
}
for my $filename (@ARGV){
    if(my $file_ext = get_ext($filename)){
        given($file_ext){
            when(/zip|7z|rar/){
                my $dirname = make_dir_name($filename);
                if (-e $dirname) {
                    my $counter = 1;
                    my $new_name;
                    do {
                        $new_name = $dirname.$counter;
                    } while (-e $new_name);
                    $dirname = $new_name;
                }
                my $program = $extract_new_dir{$_};#$_ == $file_ext
                qx{mkdir $dirname; mv $filename $dirname; cd $dirname;$program $filename};
                next;
            }
            when(/gz|bz2|xz|lzma/){
                my_system($extract{$_},$filename);
                next;
            }
            when(/tar|tgz|txz|tlz|tar\..+/){
                my_system($extract{"tar"},$filename);
                next;
            }
            default{
                say("unknown file extension $file_ext");
                exit 1;
            }
        }
    } else {
        say("No extension on file $filename");
        #query for user to continue...maybe
        exit 1;
    }
}
exit 0;

#this is basically written in c, style wise
#to do this more like perl I'd have 2 hashes with the file extenions
#as keys and the extract_program or system_args as values        
# my ($file_ext,$extract_program,$filename,@system_args);
# for $filename (@ARGV){
#     if($file_ext = get_ext($filename)){
#         given($file_ext){
#             when(/zip/){
#                 $extract_program = "unzip";
#                 goto NEW_DIR;
#             }
#             when(/rar/){
#                 $extract_program = "unrar x";
#                 goto NEW_DIR;
#             }
#             when(/7z|7zip/){
#                 $extract_program = "7z x";
#                 goto NEW_DIR;
#             }
#             when(/bz2/){
#                 @system_args=("bunzip2");
#             }
#             when(/gz/){
#                 @system_args=("gunzip");
#             }
#             when(/xz|lzma/){
#                 @system_args=("unxz");
#             }
#             when(/tar|tgz|txz|tlz|tar\..+/){
#                 @system_args=("tar","-xf");
#                 goto EXTRACT;
#             }
#             default{
#                 print("unknown file extension $file_ext\n");
#                 exit 1;
#             }
#         }
#     } else {
#         print("No extension on file $filename\n");
#         #query for user to continue...maybe
#         exit 1;
#     }
#   EXTRACT:
#     my_system(@system_args,$filename);
#     next;
#   NEW_DIR:
#     my $dirname = make_dir_name($filename);
#     if (-e $dirname) {
#         my $counter = 1;
#         my $new_name;
#         do {
#             $new_name = $dirname.$counter;
#         } while (-e $new_name);
#         $dirname = $new_name;
#     }
#     `mkdir $dirname
#      mv $filename $dirname
#      cd $dirname
#      $extract_program $filename`;
#     next;
# }

