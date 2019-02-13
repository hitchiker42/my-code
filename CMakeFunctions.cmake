## Example cmake functions and macros, most are at least somewhat useful

# Function/macro syntax
# function(name [arg1 [arg2 [arg3 ...]]])
# args can be referenced as normal variables (eg ${arg1}) or as the
# variables ARG0, ARG1, ARG2... (eg ${ARG1}), refered to as ARG#
# ARGC contains the number of arguments given
# ARGV is a list of all arguments given
# ARGN is a list of all arguments given beyond the expected arguments
#  basically ARGN is the &rest parameter.
# macro(name [arg1 [arg2 [arg3 ...]]]), similar to function
# but doesn't create a new scope and 

# Appending to a variable, as a list or string, multiple ways
function(concat result)
  # Can't just set result since we need to propagate to containing scope.
  # Even if result is empty this will still work.
  list(join "${result};${ARGN}" "" tmp) #list(join list sep out_var)
  set(${result} "${tmp}" PARENT_SCOPE)  
endfunction()
# Append to a (possibly empty) list varaible, note that since
# this is a macro we don't need PARENT_SCOPE in set
macro(append result)
  if(${result})
    # if we know result won't be empty this line is all we need
    set(${result} "${result};${ARGN}")
  else()
    set(${result} "${ARGN}")
  endif()
endmacro()
# More complex function to demonstrate argument parsing
# Adds a generic target along with dependencies and options.
function(add_target name)
  cmake_parse_arguments(ARG # prefix to prepend to keywords to make them variables
    # options, i.e keywords with no arguments
    "SHARED_LIBRARY;STATIC_LIBRARY;EXECUTABLE;EXCLUDE_ALL"
    # keywords which take exactly one argument
    "OUTPUT_NAME"
    # keywords which take multiple arguments
    "DEPENDS;COMPILE_FEATURES;COMPILE_OPTIONS;INCLUDE_DIRECTORIES;LINK_LIBRARIES"
    ${ARGN}
    )
  set(exclude_all ${ARG_EXCLUDE_ALL})
  
