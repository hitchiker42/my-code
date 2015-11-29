#!/usr/bin/env perl
use v5.18.0;
use autodie;
use File::Basename;
use Path::Tiny;
no warnings 'experimental';
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
#These need to be global since the header needs the signatures and
#documentation from all files
my $fn_signatures = [];
my $fn_doc = [];
#translate variable names likeThis into ones like_this
my $elt_type = "float";
sub un_camel_case {
  my @new_str;
  $_ = shift;
  s/([a-z0-9])([A-Z]{2,})/$1_$2/g;
  my $count = 0;
  while(s/([a-z0-9])([A-Z])(?=[a-z0-9]|$)/$1_\l$2/){
    if($count++ > 20){
      die "can't un_camel_case $_";
    }
  }
  return $_;
}
#speed doesn't really matter for this I suppose, so I may as well make
#the code cleaner by using functions
sub remove_js_constructs {
  #use @_[0] to modify the argument rather than coping it
  @_[0] =~ s/Math\.PI/M_PI/g;
  @_[0] =~ s/Math\.([^(]*?)\(/$1\(/g;
  @_[0] =~ s/===/==/g;
  @_[0] =~ s/!==/!=/g;
  @_[0] =~ s/glMatrix\.(\S*?)/$1/g;
  @_[0] =~ s/null/NULL/g;
  @_[0] =~ s/((?:mat|vec)[2-4])\.([^ ]*)/join("_",$1,un_camel_case($2))/ge;
  @_[0] =~ s/^( *)var/$1$elt_type/g;
  return @_[0];#not really necessary
}
my $js_fun_re =
    qr"(/\*(?:[^/]|/(?!\*))*?\*/\x20*)\n #Match doxygen documentation
    ^\x20*([^.]+)\.([^.]+?)(?:\.([^. ]+?))? #function name
    \x20*=\x20*function\x20*\(\N*\)\x20*{\x20*\n # matches = function(arglist) {\n
    (.*)^};? #matches the entire function body"smx;
# Translate a function declaration into C
sub translate_function {
  my $function = shift();
  #We use the documentation to get the arguments, which is why the argument
  #list itself isn't captured
  $function =~ m/$js_fun_re/;

  my ($js_doc, $class, $simd, $name, $body) = ($1,$2,$3,$4,$5);
  if($name eq ""){
    $name = $simd;
    $simd = "";
  }
#  say("js_doc = $js_doc|\nclass = $class|\n" .
#      "name = $name|\nsimd = $simd|\nbody = $body|");
  #Don't bother with simd functions
  if($simd eq "SIMD"){
    return;
  }
  my $cname = un_camel_case($name);
  my $cdoc = [];
  my $params = [];
  my $rettype;
  my $have_out = 0;
  #Translate the documentation
  #use single quotes for regexes with an '@' to avoid interpolation
  for(split(/\n/,$js_doc)){
    chomp();
    # leave lines without doxygen functions alone
    if(!m'@([^ ]*) *{([^}]*)} *([^ ]*)'p){
      push($cdoc, $_);
      next;
    }
    my $argname = $3;
    my ($predoc, $postdoc) = (${^PREMATCH}, ${^POSTMATCH});
    my $argtype;
    if(fc($2) eq fc("Number")){
      $argtype = $elt_type;
    } else {
      $argtype = "$2_t";
    }
    if($argname eq "out"){$have_out = 1;}

    if($1 =~ m/returns?/){
      $rettype = $argtype;
      if($have_out){
        my $default_ret = ($#{$params} >= 0) ? ", $params->[0][1] otherwise" : "";
        push($cdoc,
             join("","$predoc ",'@',"returns out if not NULL",
                  "$default_ret"));
      } else {
        push($cdoc, join("","$predoc ",'@',
                         "returns a $argtype,$postdoc"));
      }
    } else {
      if(!($argname =~ m/out/)){
        my $temp = ["$argtype", "$argname"];
        push($params, $temp);
        push($cdoc, join("","$predoc ",'@',"param $argname,$postdoc"));
      }
      #put param doc in a varable here and append it to cdoc before
      #the return documentation
    }
  }
  push($fn_doc, join("\n", @{$cdoc}));
  #out is the first argument in js, but the last in c
  my $arglist;
  if($have_out){
    my $temp = [$mat_type, "out"];
    push($params, $temp);
  }
  #dereferenec each element of params and join them with ','s
  my $arglist = join(", ", map {join(" ",@{$_})} @{$params});
  my $c_decl = "$rettype ${class}_${cname}($arglist)";
  push($fn_signatures, $c_decl);

  my $js_body = [];
  my $c_body = [];
  #do global substutions before splitting the body
  #$body =~ s/out/dest/g;
  @$js_body = split('^', $body);
  #A little bit of optimiziation
  $#{$c_body} = $#{$js_body};
  push($c_body, "${c_decl}\{\n");
  my $i = -1;
  # It makes life eaiser to deal with functions that allocate memory seperately
  if($name =~ m/create|clone|(?:set$)|identity|from[MVQ]|to[MVQ]/){
    #these allocate memory in the javascript version
    if($name =~ m/create|clone|fromValues/){
      my $nelts = ($mat_type =~ m/[0-9]/) || 4;
      #this is where C is better, i.e this would be js_body++
      shift($js_body);
      push($c_body, "    $mat_type out = calloc($nelts,sizeof($elt_type));\n");
    } else {
      my $nelts = ($rettype =~ m/[0-9]/) || 4;
      push($c_body, join("\n", '    if(out == NULL){',
                        "        out = calloc($nelts,sizeof($elt_type));",
                         '    }',""));
    }
    #None of these should have any javascript specific constructs
    for my $line (@{$js_body}){
      $line =~ s/^( *)var/\1$elt_type/;
      push($c_body, $line);
    }
  } else {
    #since I need to increment the loop counter in the inner
    #loop I need to use an explict for loop, rather than foreach
    for($i = 0;$i <= $#{$js_body};$i++){ 
    #   #translate variable declarations
    #   if($js_body->[$i] =~ s/^( *)var/\1$elt_type/){
    #     do {
    #       remove_js_constructs($js_body->[$i]);
    #       push($c_body, $js_body->[$i]);
    #     } while(!($js_body->[$i++] =~ m/; *$/) && ($i <= $#{$js_body}));
    #   } else {
        remove_js_constructs($js_body->[$i]);
        if($have_out){
          #If the output parameter is NULL replace it in a sane way
          if($params->[0][0] eq $mat_type){
            #use the first argument if it's
            push($c_body, join("\n",'    if(out == NULL){',
                               "        out = $params->[0][1];",
                               '    }',""));
          } else {
            push($c_body, join("\n",'    if(out == NULL){',
                               "        out = ${class}_create();",
                               '    }',""));
          }
          #this is a bit lazy, but it should work
          $have_out = 0;
        }
        #It might seem like we could just process the rest of the function
        #here, but there could be more variable definations
        push($c_body, $js_body->[$i]);
      }
    
  }
  return join("", @{$c_body}, "}\n");
}
# Takes a string of javascript code  and a base matrix/vector type
# (i.e (mat|vec)[2-4] and outputs equivlent C code to $type.c where
# $type is the second argument
sub translate_file {
  my $js_code = shift;;#javascript file in a string
  my $type = shift;
  $mat_type = "${type}_t";
  my $fns = [];
  while($js_code =~ m"/\*(?:[^/]|/(?!\*))*?\*/ *\n($type\.\N*?) *= *function *\(\N*\) *\{.*?^};?"smpg){
    say("Translating $1");
    #Ignore some problamatic functions
    next if($1 =~ m/str/ || 
            $1 =~ m/perspectiveFromFieldOfView/ ||
            $1 =~ m/random/);
#    say("match = ${^MATCH}");
    my $fun = translate_function(${^MATCH});
    if($fun){
      push($fns, $fun);
    }
    say("Translated $1");
  }
  return $fns;
}
# Usage js_to_c.pl js_dir c_dir
my $js_dir = shift;
my $c_dir = shift;
die("Usage: js_to_c.pl js_dir c_dir") if(!$c_dir);
opendir(my $dh, $js_dir);
if(!(-d $c_dir)){
  die("Error $c_dir exists and is not a directory") if(-e $c_dir);
  #just use the shell, it's eaiser
  `mkdir -p $c_dir`;
}
my $doc_index = 0;
sub write_c_file {
  my $fns = shift;
  my $filename = shift;
  open(my $fh, ">", $c_dir . '/' . $filename);
  $\ = "\n";
  print($fh '#include "gl_matrix.h"');
  print($fh '#define min(a,b) (a<b ? a : b)');
  print($fh '#define max(a,b) (a>b ? a : b)');
  for my $fn (@{$fns}){
    print($fh $fn_doc->[$doc_index++]);
    print($fh $fn);
  }
  $\ = undef;
  close($fh);
}
#my $file_functions = {};

while(readdir $dh){
  next unless(m/((?:(?:mat|vec)[0-9]+)|quat)(\..*)/);
  my ($filename, $suffix) = ($1,$2);
  my $js_path = path($js_dir, $filename . $suffix);
  my $js_code = $js_path->slurp({binmode => ":raw"});
  my $c_functions = translate_file($js_code, $filename);
  write_c_file($c_functions, $filename . ".c");
}
my $header_preamble = join("\n","#ifndef _GL_MATRIX_H_",
                           "#define _GL_MATRIX_H_",
                           "typedef $elt_type *mat2_t;",
                           "typedef $elt_type *mat3_t;",
                           "typedef $elt_type *mat4_t;",
                           "typedef $elt_type *vec2_t;",
                           "typedef $elt_type *vec3_t;",
                           "typedef $elt_type *vec4_t;",
                           "typedef vec4_t quat_t;",
                           "typedef quat_t quat4_t;",
                           "typedef mat2_t mat2d_t;",
                           "#include <tgmath.h>",
                           "#include <stdlib.h>",
                           "#define EPSILON 0.000001",
                           "#define RANDOM drand48");
my $header_epilogue = "#endif\n";
open(my $c_header, ">", $c_dir . "/gl_matrix.h");
print($c_header $header_preamble);
while((my $decl = shift($fn_signatures)) &&
      (my $doc = shift($fn_doc))){
  print($c_header $doc);
  print($c_header "\n$decl;\n")
}
print($c_header $header_epilogue);                    
# Local Variables:
# perl-indent-level: 2
# End:
