#!/usr/bin/env perl
use v5.18.0;
use autodie;
no warnings "experimental";
use Getopt::Long qw(:config gnu_getopt);
use File::Temp qw(tempdir);
sub usage {
    say("Usage: sizeof.pl [options] types...");
    say("    Options:");
    say("      -I|--include includes: Comma seperated list of headers to include.");
    say("      -f|--format format-string: Alternate format string, receives 2 arguments");
    say("        the type name as a string and size of the type as an integer.");
    say("      -C|--cc command: Command used to compile the generated C file");
    say("        this should compile 'sizeof.c' into an executable file named a.out.");
    exit(0);
}
my $includes = ["stdio.h", "stdlib.h", "unistd.h", "sys/types.h"];
my $extra_includes;
my $format_str = 'sizeof(%s) = %d\n';
my $CC = $ENV{'CC'} || "gcc";
my $compile_command = "$CC -O2 -g sizeof.c -o a.out";
if(scalar(@ARGV) == 0){
    usage();
}
GetOptions("includes|I=s" => \$extra_includes,
           "format|f=s" => \$format_str,
           "help|h" => \&usage);
if(defined($extra_includes)){
    push($includes->@*, split(/\s*,\s*/,$extra_includes));
}
my $include_directives = join("\n", map {"#include <$_>"} $includes->@*);
my $print_statements = join("\n", map {"print_sizeof($_);"} @ARGV);
my $code = <<EOF;
$include_directives
#define print_sizeof(type)\\
  printf("$format_str", #type, sizeof(type))

int main(){
  $print_statements
}
EOF
# say($code);
## We're going to create a temporary directory to store the source and 
## executable files, this will let us use normal file names, but have
## the same safety as using temp files.
 
umask(0077); ## Make sure no one else can access the directory
my $tempdir = tempdir();
chdir($tempdir);

open(my $fh, ">", "sizeof.c");
print($fh $code);
system($compile_command);
exec './a.out';
