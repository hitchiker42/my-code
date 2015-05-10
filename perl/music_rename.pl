#! /usr/bin/env perl
#NOTE: this isn't done.
use v5.18.0;
no warnings 'experimental';
use File::Basename;
use File::LibMagic;
use File::stat;
use Music::Tag (traditional => 1, pbp => 1);
my $mime2ext_database = {};
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
#returns the extension of the given file, if the file has an extension
#it simply returns it, i.e it doesn't attempt to verify it, only using libmagic
#if the file has no extension
sub file_ext {    
    my $file=shift;
    if($file =~ /$.*?(\w+(\.\w+)*)$/){
        return $1;
    } else {
        my ($mime)=split(';',$magic->checktype_filename($file));
        return $mime2ext_database{$mime}[0];
    }
    
}
sub get_music_info {
    my $filename = shift;
    my $ext = file_ext($filename);
    my $info = Music::Tag->new($filename, {quiet => 1});
#for each type of info (artist, album, title, etc...)
#the data can be accessed via $info->data(), and set by $info->data('new')
#Or accessed via $info->get_data() and set by $info->set_data('new')
    if(!$info->plugin('.*')){#this probaly won't work as is
        $info->add_plugin(uc($ext));
    }
    $info->add_plugin("MusicBrainz");
    #this next line does a lot, it does the metadata parsing, and then
    #querys the online MusicBrainz database
    $info->get_tag();
    #updates the tags in the actual file, should be an option to enable/disable this
    $info->set_tag();
    $info->close();#close any filehandles, etc...
    my $info_hash = {
        album => $info->album(),
        artist => $info->artist(),
        title => $info->title(),
        year => $info->year(),
        filename => $filename,
        ext => $ext
    };
    return $info_hash;
}

sub create_new_filename {
    my ($info, $pattern) = @_;
    my $filename = $pattern; #apperently perl copies string by value by default
    #I'll have to canonicalize the meanings of different specifiers
    $filename =~ s/%t/$info->title()/g;
    $filename =~ s/%A/$info->album()/g;
    $filename =~ s/%a/$info->artist()/g;
    $filename =~ s/%y/$info->year()/g;
    #Maybe add more
    return $filename;
}

#default mass rename of files, can alternatively use patterns to
#specify how to rename files. 2 hardlinks to each file are made,
#one in the Artist/song->artist/song->album directory
#and one in the Album/song->album directory
#this essentally turns the filesystem into a simple database
sub re_orginize_files {
    my $songs = shift; #array ref of songinfo
    for my $song ($songs){
        #maybe eventually switch to a perl version of all this but for now just use a shell
        qx(mkdir -p Artist/${song->artist};
           mkdir -p Album/${song->album};
           ln $song->filename Artist/${song->artist}/${song->album}/${song->title}.${song->ext};
           ln $song->filename Album/${song->album}/${song->title}.{$song->ext};);
    }
}

sub main {
    my $dirs = shift;
    my $dest_dir = shift;
    my $music_db = [];
    for $dir (@{$dir}){
        for $file (`ls $dir`){#FIXME: This won't work
            my $info = get_music_info($file);
            push($music_db, $info);
        }
    }
    chdir($dest_dir);
    re_orginize_files($music_db);
}
