struct datatype_defn{
  const char* type_id;
  symref *of_val;
};
struct ctype {
  const char* nil;const char* real;const char* word;
  const char* cons;const char* ref;const char* bool;
  const char* fun;const char* tuple;const char* opt;
};
const struct ctype ctypes={"_nil","double","unsigned long",
                           "cons*","datatype*","enum boolean",
                           "func_t","tuple","datatype*"};
char* get_ctype(type mltype){
  switch (type){
    case _nil: return ctypes.nil;
    case _real: return ctypes.real;
    case _word: return ctypes.word;
    case _cons: return ctypes.cons; 
    case _ref: return ctypes.ref; 
    case _bool: return ctypes.bool;
    case _fun: return ctypes.fun;
    case _tuple: return ctypes.tuple; 
    case _opt: return ctypes.opt;
  }
}
void mk_datatype(char* name,datatype_defn* values,int len){
  int i,have_union=0;
  fprintf(/*some file*/,"typedef enum enum_%s enum_%s;\n",name,name);

  fprintf(/*some file*/,"typedef struct %s %s;\n",name,name);
  fprintf(/*some file*/,"enum enum_%s {\n",name);
  for(i=0;i<len-1;i++){
    fprintf(/*some file*/,"_%s = %d,\n",values[i].type_id,i);
    if(values[i].of_val.tag != _nil){
      if(!have_union){
        fprintf(/*some other file*/,
                "typedef union union_%s union_%s;\n",name,name);
        fprintf(/*some other file*/,"union union_%s {\n",name);
      }
      fprintf(/*some other file*/,"%s %s;\n",
    }
  } fprintf(/*some file*/,"_%s = %d};\n",values[i++].type_id,i);
