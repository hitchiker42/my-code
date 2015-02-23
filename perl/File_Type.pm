package File::Type;
use v5.18;
use FFI::Raw;
use autodie;
my @ctypes = ("void","ptr","int","uint","char","uchar",
              "short","ushort","float","double",
              "long","ulong","int64","uint64","str");
my ($void,$ptr,$int,$uint64,$str) =
    (FFI::Raw::void(),FFI::Raw::ptr(),FFI::Raw::int(),
     FFI::Raw::uint64(),FFI::Raw::str());
my $NULL = FFI::Raw::Ptr::new(0);
use constant {
    MAGIC_NONE	=> 0x000000,
    MAGIC_DEBUG	=> 0x000001,
    MAGIC_SYMLINK	=> 0x000002,
    MAGIC_COMPRESS	=> 0x000004,
    MAGIC_DEVICES	=> 0x000008,
    MAGIC_MIME_TYPE	=> 0x000010,
    MAGIC_CONTINUE	=> 0x000020,
    MAGIC_CHECK	=> 0x000040,
    MAGIC_PRESERVE_ATIME=> 0x000080,
    MAGIC_RAW	=> 0x000100,
    MAGIC_ERROR	=> 0x000200,
    MAGIC_MIME_ENCODING=> 0x000400,
    MAGIC_MIME => (0x10|0x400),
    MAGIC_APPLE	=> 0x000800,
    MAGIC_NO_CHECK_COMPRESS=> 0x001000,
    MAGIC_NO_CHECK_TAR=> 0x002000,
    MAGIC_NO_CHECK_SOFT=> 0x004000,
    MAGIC_NO_CHECK_APPTYPE=> 0x008000,
    MAGIC_NO_CHECK_ELF=> 0x010000,
    MAGIC_NO_CHECK_TEXT=> 0x020000,
    MAGIC_NO_CHECK_CDF=> 0x040000,
    MAGIC_NO_CHECK_TOKENS=> 0x100000,
    MAGIC_NO_CHECK_ENCODING => 0,
}
sub GenFFI {
    my $lib = shift;
    my $funs = [];
    while (my ($name, $rettype, @argtypes) = shift) {
        push($funs, FFI::Raw::new($lib,$name,$rettype,@argtypes));
    }
    return $funs;
}
sub FFI_call {
    my $fun = shift;
    return $fun->call(@_);
}
sub magic_init {
    GenFFI("libmagic",
           ("magic_open",$ptr,$int),
           ("magic_close",$void,$ptr),
           ("magic_error",$str,$ptr),
           ("magic_errno",$int,$ptr),
           ("magic_descriptor",$str,$ptr,$int),
           ("magic_file",$str,$ptr,$str),
           ("magic_buffer",$str,$ptr,$ptr,$uint64),
           ("magic_setflags",$int,$ptr,$int),
           ("magic_check",$int,$ptr,$str),
           ("magic_compile",$ptr,$str),
           ("magic_list",$int,$ptr,$str),
           ("magic_load",$int,$ptr,$str),
           ("magic_version",$int,$void));
}
my ($magic_open,$magic_close,$magic_error,$magic_errno,$magic_descriptor,
    $magic_file,$magic_buffer,$magic_setflags,$magic_check,$magic_compile,
    $magic_list,$magic_load,$magic_version) = magic_init();
sub magic_new {
    my $flags = shift || MAGIC_NONE;
    my $filenames = (shift || $NULL);
    if ($filenames != $NULL) {
        if (FFI_call($magic_check, $filenames) < 0) {
            $filenames = $NULL;
            #warn here
        }
    }
    my $magic_cookie = FFI_call($magic_open, $flags);
    if (FFI_call($magic_load, $magic_cookie, $filenames) < 0) {
        return;
    }
    return $magic_cookie;
}
my $magic_static = magic_new();

#returns a reference to a hash whoes keys are file extensions
#and values are the mime type assoicated with that extension
sub gen_mime_globs {
    my $mime_file = shift || "/usr/share/mime/globs";
    my $mime_globs = {};
    open(my $mime_fh,"<",$mime_file);    
    while ($_ = readline($mime_fh)) {
        m{^(\w+)/([^:]+):\*\.(.+)$};
        $mime_globs->{$3} = ($1,$2);
    }
    return $mime_globs;
}

sub get_mime_type {
    my $magic = shift;
    my $file;
    if (ref($magic)) {
        $file = shift;
    } else {
        $file = $magic;
        $magic = $magic_static;
    }
    FFI_call($magic_setflags, $magic, MAGIC_MIME);
    my $retval = FFI_call($magic_file, $magic, $file);
    FFI_call($magic_setflags, $magic, MAGIC_NONE);
    return $retval;
}

sub mime_to_ext {
    my $mime_type = shift;
    $mime_type =~ m{^(\w+)/(.+)};
    my ($top_level,$subtype) = ($1,$2);
}
