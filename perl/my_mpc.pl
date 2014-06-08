#!/usr/bin/env perl
use v5.18.0;
no warnings 'experimental';
use autodie;
use Socket;
use Getopt::Long;
use vars '$mpd_socket';#dynamicly scoped
use subs 'chomp';
sub chomp {
    my $line=shift;
    return CORE::chomp($line);
}
Getopt::Long::Configure("gnu_getopt");
my $host = "localhost";
my $port = 6600;
my %mpd_dispatch_table;
#use of qr compiles the regex when the variable is defined, otherwise
#the regex would be compiled each time it was used
my $mpd_error_re=qr'ACK \[(\d+)@(\d+)\] \{(\w+)\} (\w+)\n';
my $mpd_info_re=qr'([a-zA-Z]+): (.+)';
my @mpd_commands=[
     #query status
    'clearerror','currentsong','idle','status','stats',
    #playback options
    'consume','crossfade','mixrampdb','mixrampdelay','random','repeat',
    'setvol','single','replay_gain_mode','replay_gain_status','volume',
    #controling playback
    'next','pause','play','playid','previous','seek','seekid','seekcur','stop',
    #current playlist
    'add','addid','clear','delete','move','moveid','playlistfind',
    'playlistinfo','playlistsearch','plchangesposid','prio','prioid',
    'shuffle','swap','swapid','addtagid','cleartagid',
    #stored playlist
    'listplaylist','listplaylistinfo','listplaylists','load','playlistadd',
    'playlistclear','playlistdelete','playlistmove','rename','rm','save',
    #music data base
    'count','find','findadd','list','listall','listallinfo','listfiles',
    'lsinfo','readcomments','search','searchadd','searchaddpl',
    'update','rescan',
    #misc
    'kill','close','ping','password','outputs','disableoutput',
    'enableoutput','toggleoutput'
];
@mpd_dispatch_table{@mpd_commands} =
  map(\&gen_mpd_request_fun->($_),@mpd_commands);
$mpd_dispatch_table{'prev'}=$mpd_dispatch_table{'previous'};
sub regex_match { #probably significantly slower than using m//
    my ($re,$str,$opts)=@_;
    
    $str =~ m{$re}"p$opts"

    return ${^MATCH};
}
    
sub getopt_long { #shortopts, longopts, options
    my($shortopts,$longopts,@args)=@_;
    return
      split(/ /,strip(qx"getopt -u -o $shortopts --long $longopts -- @args"));
}
sub socket_connect {        #hostname,port
    my($host,$port)=@_;
    my $socket;
    my $ipaddr = inet_aton($host) || return;
    my $sockaddr=pack_sockaddr_in($port,$ipaddr);
    socket($socket,PF_INET(),SOCK_STREAM(),getprotobyname("tcp"));
    #if the call to socket failed the following call to connect will also
    #fail, so a seperate if(!$socket){return undef;} is unecessary
    if (connect($socket,$sockaddr)) {
        return $socket;
    } else {
        return;
    }
}
#if I was using this as a more conventional function I'd have the socket handle
#come first, but this way is eaiser
sub mpd_request {#command, socket_handle, args
    my ($command,$socket_handle,@args) = @_;
    print($socket_handle,$command,@args);
    my (@responce,$line,$i);
    while($line=readline($socket_handle)){
        @responce[$i++]=chomp($line);
    }
    if($line =~ $mpd_error_re){
        say(STDERR(),"mpd error in command ",$3,": ",$4);
    } else {
        return \@responce;
    }
}
sub gen_mpd_request_fun {#command
    my $command=shift;
    return sub{return mpd_request($command,$mpd_socket,@_);}
}
sub check_opts {
    my ($numargs,$reqargs,$optargs,$restarg)=@_;
    #insure than numargs is an acceptable number of arguments
    #for a function with the prototype corrsponding to the
    #number of required/optional/rest arguments specified
    if($numargs<$reqargs || (!$restarg && ($numargs > $reqargs+$optargs))){
        return 0;
    } else {
        return 1;
    }
}
sub mpd_connect {#$hostname, $port
    $mpd_socket = &socket_connect->(@_);
    if(!$mpd_socket){
        die (sprintf("Unable to connect to mpd server at %d:%d",@_));
    }
    my $line=chomp(readline($mpd_socket));
    if($line =~ /OK MPD [0-9.]+/){
        return;
    } else {
        die (sprintf("Did not recieve responce from mpd server at %d:%d",@_));
    }
}
#these two functions will only work for info on one song, or for the status command
sub parse_mpdinfo {#takes an array reference
    my $aref=shift;
    my %mpdinfo;
    for my $info (@$aref) {
        $info =~ $mpd_info_re;
        $mpdinfo{$1}=$2;
    }
    return \%mpdinfo;
}
sub read_mpdinfo {#takes a filehandle
    my $fd=shift;
    my %mpdinfo;
    while(my $info=chomp(readline($fd))){
        $info =~ $mpd_info_re;
        $mpdinfo{$1}=$2;
    }
    return \%mpdinfo
}
#read the current play list into an array whoes elements
#are hashes containg information about the song at that
#position in the playlist
sub parse_playlistinfo {    
    my $fd=shift;
    my @playlist; #array of hashes
    my $i=0;
    while(my $info=chomp(readline($fd))){
        $info =~ $mpd_info_re;
p        $playlist[$i]->{$1}=$2;
        if($1 eq 'Id'){
            $i++;
        }
    }
    return \@playlist;
}
sub format_songinfo {
    my ($format,%info)=@_;
    
   
            
        
    
GetOptions('host|h=s' => \$host,
           'port|p=i' => \$port);
given ($ARGV[0]){
    #don't take any arguments and give no intresting output
    when(['next','prev','stop','clear','kill']){
        $mpd_dispatch_table{$_}->();
    }
}

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
