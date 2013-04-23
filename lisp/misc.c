/*	Compiler: ECL 12.12.1                                         */
/*	Date: 2013/4/22 21:06 (yyyy/mm/dd)                            */
/*	Machine: Linux 3.8.5-201.fc18.x86_64 x86_64                   */
/*	Source: misc.lisp                                             */
#include <ecl/ecl-cmp.h>
#include "misc.eclh"
/*	function definition for SEQ                                   */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L1seq(cl_narg narg, ...)
{
 cl_object T0;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  cl_object V1;
  cl_object V2;
  cl_object V3;
  ecl_va_list args; ecl_va_start(args,narg,narg,0);
  {
   ecl_ihs_push(cl_env_copy,&ihs,VV[0],_ecl_debug_env);
   {
    cl_object keyvars[6];
    cl_parse_key(args,3,L1seqkeys,keyvars,NULL,FALSE);
    ecl_va_end(args);
    if (Null(keyvars[3])) {
     V1 = ecl_make_fixnum(0);
    } else {
     V1 = keyvars[0];
    }
    if (Null(keyvars[4])) {
     V2 = ecl_make_fixnum(10);
    } else {
     V2 = keyvars[1];
    }
    if (Null(keyvars[5])) {
     V3 = ecl_make_fixnum(1);
    } else {
     V3 = keyvars[2];
    }
   }
   {
    cl_object V4;                                 /*  I               */
    cl_object V5;
    cl_object V6;
    {
     T0 = cl_realp(V1);
     if (ecl_unlikely(!((T0)!=ECL_NIL)))
         FEwrong_type_argument(ECL_SYM("REAL",703),V1);
     V4 = V1;
    }
    {
     T0 = cl_realp(V2);
     if (ecl_unlikely(!((T0)!=ECL_NIL)))
         FEwrong_type_argument(ECL_SYM("REAL",703),V2);
     V5 = V2;
    }
    {
     T0 = cl_realp(V3);
     if (ecl_unlikely(!((T0)!=ECL_NIL)))
         FEwrong_type_argument(ECL_SYM("REAL",703),V3);
     V6 = V3;
    }
    {
     static const struct ecl_var_debug_info _ecl_descriptors[]={
     {"#:LOOP-STEP-BY1",_ecl_object_loc}
     ,{"#:LOOP-LIMIT0",_ecl_object_loc}
     ,{"COMMON-LISP-USER::I",_ecl_object_loc}};
     const cl_index _ecl_debug_info_raw[]={
     (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V6),(cl_index)(&V5),(cl_index)(&V4)};
     ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,5,,);
     ihs.lex_env = _ecl_debug_env;
     {
      cl_object V7;
      cl_object V8;
      V7 = ecl_list1(ECL_NIL);
      V8 = V7;
      {
       static const struct ecl_var_debug_info _ecl_descriptors[]={
       {"#:LOOP-LIST-TAIL3",_ecl_object_loc}
       ,{"#:LOOP-LIST-HEAD2",_ecl_object_loc}};
       const cl_index _ecl_debug_info_raw[]={
       (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V8),(cl_index)(&V7)};
       ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
       ihs.lex_env = _ecl_debug_env;
L12:;
       if (!(ecl_number_compare(V4,V5)>0)) { goto L14; }
       goto L13;
L14:;
       T0 = V8;
       V8 = ecl_list1(V4);
       cl_rplacd(T0, V8);
       V4 = ecl_plus(V4,V6);
       goto L12;
L13:;
       value0 = ecl_cdr(V7);
       cl_env_copy->nvalues = 1;
       ecl_ihs_pop(cl_env_copy);
       return value0;
      }
      ihs.lex_env = _ecl_debug_env;
     }
    }
    ihs.lex_env = _ecl_debug_env;
   }
  }
 }
}
/*	function definition for INTERLEAVE                            */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L2interleave(cl_object V1, cl_object V2)
{
 cl_object T0;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::LIST1",_ecl_object_loc}
   ,{"COMMON-LISP-USER::LIST2",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[1],_ecl_debug_env);
TTL:
   {
    cl_object V3;                                 /*  LET4            */
    cl_object V4;                                 /*  LET5            */
    cl_object V5;                                 /*  TEMP            */
    cl_object V6;                                 /*  A               */
    cl_object V7;                                 /*  B               */
    V3 = cl_copy_list(V1);
    V4 = cl_copy_list(V2);
    V5 = ECL_NIL;
    V6 = V3;
    V7 = V4;
    {
     static const struct ecl_var_debug_info _ecl_descriptors[]={
     {"COMMON-LISP-USER::B",_ecl_object_loc}
     ,{"COMMON-LISP-USER::A",_ecl_object_loc}
     ,{"COMMON-LISP-USER::TEMP",_ecl_object_loc}
     ,{"COMMON-LISP-USER::LET5",_ecl_object_loc}
     ,{"COMMON-LISP-USER::LET4",_ecl_object_loc}};
     const cl_index _ecl_debug_info_raw[]={
     (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V7),(cl_index)(&V6),(cl_index)(&V5),(cl_index)(&V4),(cl_index)(&V3)};
     ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,7,,);
     ihs.lex_env = _ecl_debug_env;
L7:;
     if (!(ecl_equal(ECL_NIL,V6))) { goto L9; }
     goto L8;
L9:;
     {
      cl_object V8;
      V8 = V6;
      {
       static const struct ecl_var_debug_info _ecl_descriptors[]={
       {"#:G5",_ecl_object_loc}};
       const cl_index _ecl_debug_info_raw[]={
       (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V8)};
       ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
       ihs.lex_env = _ecl_debug_env;
       {
        cl_object V9;
        V9 = ecl_car(V8);
        {
         static const struct ecl_var_debug_info _ecl_descriptors[]={
         {"#:G6",_ecl_object_loc}};
         const cl_index _ecl_debug_info_raw[]={
         (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V9)};
         ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
         ihs.lex_env = _ecl_debug_env;
         V8 = ecl_cdr(V8);
         V6 = V8;
         T0 = V9;
        }
        ihs.lex_env = _ecl_debug_env;
       }
      }
      ihs.lex_env = _ecl_debug_env;
     }
     V5 = CONS(T0,V5);
     {
      cl_object V8;
      V8 = V7;
      {
       static const struct ecl_var_debug_info _ecl_descriptors[]={
       {"#:G8",_ecl_object_loc}};
       const cl_index _ecl_debug_info_raw[]={
       (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V8)};
       ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
       ihs.lex_env = _ecl_debug_env;
       {
        cl_object V9;
        V9 = ecl_car(V8);
        {
         static const struct ecl_var_debug_info _ecl_descriptors[]={
         {"#:G9",_ecl_object_loc}};
         const cl_index _ecl_debug_info_raw[]={
         (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V9)};
         ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
         ihs.lex_env = _ecl_debug_env;
         V8 = ecl_cdr(V8);
         V7 = V8;
         T0 = V9;
        }
        ihs.lex_env = _ecl_debug_env;
       }
      }
      ihs.lex_env = _ecl_debug_env;
     }
     V5 = CONS(T0,V5);
     goto L7;
L8:;
     value0 = cl_reverse(V5);
     ecl_ihs_pop(cl_env_copy);
     return value0;
    }
    ihs.lex_env = _ecl_debug_env;
   }
  }
 }
}

#include "misc.data"
#ifdef __cplusplus
extern "C"
#endif
ECL_DLLEXPORT void init_fas_CODE(cl_object flag)
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 cl_object *VVtemp;
 if (flag != OBJNULL){
 Cblock = flag;
 #ifndef ECL_DYNAMIC_VV
 flag->cblock.data = VV;
 #endif
 flag->cblock.data_size = VM;
 flag->cblock.temp_data_size = VMtemp;
 flag->cblock.data_text = compiler_data_text;
 flag->cblock.data_text_size = compiler_data_text_size;
 flag->cblock.cfuns_size = compiler_cfuns_size;
 flag->cblock.cfuns = compiler_cfuns;
 flag->cblock.source = make_constant_base_string("/home/tucker/Repo/my-code/lisp/misc.lisp");
 return;}
 #ifdef ECL_DYNAMIC_VV
 VV = Cblock->cblock.data;
 #endif
 Cblock->cblock.data_text = "@EcLtAg:init_fas_CODE@";
 VVtemp = Cblock->cblock.temp_data;
 ECL_DEFINE_SETF_FUNCTIONS
 ecl_function_dispatch(cl_env_copy,VV[2])(10, VVtemp[0], ECL_NIL, ECL_NIL, VVtemp[1], ECL_NIL, ECL_NIL, VVtemp[2], ECL_NIL, ECL_NIL, ECL_NIL) /*  DODEFPACKAGE */;
 ecl_cmp_defun(VV[3]);                            /*  SEQ             */
 ecl_function_dispatch(cl_env_copy,VV[4])(3, VV[0], ECL_SYM("FUNCTION",396), VVtemp[3]) /*  SET-DOCUMENTATION */;
 ecl_cmp_defun(VV[8]);                            /*  INTERLEAVE      */
}
