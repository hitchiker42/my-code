#!/usr/bin/env perl
use v5.18.0;
no warnings 'experimental';
use autodie;
use Socket;
use Getopt::Long;
Getopt::Long::Configure("gnu_getopt");
my $host = "localhost";
my $port = 6600;
my $mpd_error_re='ACK \[(\d+)@\(d+)\] \{(\w+)\} (\w+)\n'
sub getopt_long($,$,@){ #shortopts, longopts, options
    my($shortopts,$longopts,@args)=@_;
    return split(/ /,strip(qx("getopt -u -o $shortopts --long $longopts -- @args")));
}
sub socket_connect($,$){        #hostname,port
    my($host,$port)=@_;
    my $socket;
    my $ipaddr = inet_aton($host) || return undef;
    my $sockaddr=pack_sockaddr_in($port,$ipaddr);
    socket($socket,PF_INET(),SOCK_STREAM(),getprotobyname("tcp"));
    #if the call to socket failed the following call to connect will also
    #fail, so a seperate if(!$socket){return undef;} is unecessary
    if (connect($socket,$sockaddr)) {
        return $socket;
    } else {
        return undef;
    }
}

GetOptions('host|h=s' => \$host,
           'port|p=i' => \$port);

=head1 MPD_SPEC

This is just an eaiser way to do a block comment, it's not really documentation.

MPD Responces,

only 2 types of responces:
OK : success

ACK \[(\d+)@\(d+)\] \{(\w+)\} (\w+)\n
$1 = error number
$2 = command list offset (the line in a command list that caused
the error, offsets are 0 based)
$3 = name of the command that casued the error
$4 = error message

MPD Input,

multiple commands can be submitted at once via a command list of the form

command_list_begin or command_list_ok_begin
<list of commands>
command_list_end

if command_list_ok_begin is used print list_OK after each successful command

numeric ranges take the form of start:END meaning [start,END)

info:

all information is given as a series of lines of key-value pairs of the form
(key: value\n)+


=cut
