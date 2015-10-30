#!/usr/bin/env perl
use v5.18.0;
# Basic Information:
# The javascript library is mostly object oriented, the C translation isn't
# The javascript functions take the output matrix first, the C functions
#  take it last, and make it optional

# Basic Conventions:
# In general A javascript funcion is declared:
#   type\.((?:SIMD)|(?:scalar)\.)operation = function(out, inputs)

# The current element and matrix types, along with an optional type prefix
# i.e 'd' to turn mat4 into dmat4
my ($elt_type, $type_prefix, $mat_type);#mat_type could be a vector type as well
# The strings in this array don't have trailing semicolons, so to write these to
# the header make sure to append a ';'
my $fn_signatures = [];
my $fn_doc = [];

# Translate a function declaration into C
sub translate_function {
    my $function = shift();
    #We use the documentation to get the arguments, which is why the argument
    #list itself isn't captured
    $function =~
        m"(/\*\*.*?\*/\x20*\n) #Match doxygen documentation
          ^\x20*(....)\.((?:(?:SIMD)|(?:scalar))\.)([^ ]+) #function signature
          [^{]+{\x20*\n # matches = function(arglist) {\n
          (.*)} #matches the entire function body"smx;
    my ($js_doc, $class, $simd, $name, $body) = ($1,$2,$3,$4,$5);
    #Don't bother with simd functions
    if($simd eq "SIMD"){
        return "";
    }
    my $cdoc = [];
    my $params = [];
    my $rettype;
    #Might need to \ the '@'s
    my $have_out = 0;
    #Translate the documentation
    for(split(/\n/,$js_doc)){
        chomp();
        # leave lines without doxygen functions alone
        if(!m/@([^ ]*) *{([^}]*)} *([^ ]*)/p){
            push($cdoc, $_);
            next;
        }
        my $argname = $3;
        my ($predoc, $postdoc) = (${^PREMATCH}, ${^POSTMATCH});
        my $argtype;
        if($2 eq "Number"){
            $argtype = $elt_type;
        } else {
            $argtype = "$2_t";
        }
        if($argname eq "out"){$have_out = 1;}
        push($cdoc,"$predoc @$1 $3 - a $argtype,$postdoc");
        if($1 =~ m/returns?/){
            $rettype = $argtype;
            if($have_out){
                push($cdoc,
                     "$predoc @returns dest if not NULL, $params->[0][1] otherwise")
            } else {
                push($cdoc, "$predoc @returns a $argtype,$postdoc");
            }
        } else {
            push($params, ["$argtype", "$argname"]);#This should push an aref
            push($cdoc, "$predoc @param $argname,$postdoc");
        }
    }
    push($fn_doc, join("\n", @{$cdoc}));
    if($have_out){
        push($params, [$mat_type, "dest"]);
    }
    my $arglist = join(", ", map {@$_} $params);#might need @ infront of params
    my $c_decl = "$rettype ${class}_${name}($arglist)";
    push($fn_signatures, $c_decl);

    my @js_body = [];
    my $c_body = [];
    #do global substutions before splitting the body
    $body =~ s/out/dest/g
    $js_body = split('^', $body);
    push($c_body, "$c_decl{\n");
    my $i = 0;
    # It makes life eaiser to deal with functions that allocate memory seperately
    if(name ~= m/create|clone|set|identity|from|to/){
    } else {
        while($i < scalar(@{$js_body}){
            if($js_body->[i] =~ s/^( *)var/\1$elt_type/){
                do {
                    push($c_body, $js_body->[$i]);
                } while(!($js_body->[$i++] =~ m/; *$/));
            }
            
            
    
}
