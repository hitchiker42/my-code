/*	Compiler: ECL 12.12.1                                         */
/*	Date: 2013/4/22 21:06 (yyyy/mm/dd)                            */
/*	Machine: Linux 3.8.5-201.fc18.x86_64 x86_64                   */
/*	Source: calculus.lisp                                         */
#include <ecl/ecl-cmp.h>
#include "calculus.eclh"
/*	function definition for DOT-PROD                              */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L1dot_prod(cl_object V1, cl_object V2)
{
 cl_object T0, T1, T2, T3;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::X",_ecl_object_loc}
   ,{"COMMON-LISP-USER::Y",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[0],_ecl_debug_env);
TTL:
   if (ecl_unlikely(!ECL_VECTORP(V1))) FEtype_error_vector(V1);
   if (ecl_unlikely(!ECL_VECTORP(V2))) FEtype_error_vector(V2);
   {
    T0 = cl_array_total_size(V1);
    {
     cl_object V3;                                /*  I               */
     cl_fixnum V4;
     cl_fixnum V5;
     V3 = ECL_NIL;
     V4 = 0;
     V5 = 0;
     {
      static const struct ecl_var_debug_info _ecl_descriptors[]={
      {"#:LOOP-ACROSS-LIST2",_ecl_fixnum_loc}
      ,{"#:LOOP-ACROSS-INDEX-1",_ecl_fixnum_loc}
      ,{"COMMON-LISP-USER::I",_ecl_object_loc}};
      const cl_index _ecl_debug_info_raw[]={
      (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V5),(cl_index)(&V4),(cl_index)(&V3)};
      ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,5,,);
      ihs.lex_env = _ecl_debug_env;
      {
       cl_object V6;                              /*  J               */
       cl_fixnum V7;
       cl_fixnum V8;
       V6 = ECL_NIL;
       V7 = 0;
       V8 = 0;
       {
        static const struct ecl_var_debug_info _ecl_descriptors[]={
        {"#:LOOP-ACROSS-LIST5",_ecl_fixnum_loc}
        ,{"#:LOOP-ACROSS-INDEX-4",_ecl_fixnum_loc}
        ,{"COMMON-LISP-USER::J",_ecl_object_loc}};
        const cl_index _ecl_debug_info_raw[]={
        (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V8),(cl_index)(&V7),(cl_index)(&V6)};
        ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,5,,);
        ihs.lex_env = _ecl_debug_env;
        {
         cl_object V9;
         cl_object V10;
         V9 = ecl_list1(ECL_NIL);
         V10 = V9;
         {
          static const struct ecl_var_debug_info _ecl_descriptors[]={
          {"#:LOOP-LIST-TAIL7",_ecl_object_loc}
          ,{"#:LOOP-LIST-HEAD6",_ecl_object_loc}};
          const cl_index _ecl_debug_info_raw[]={
          (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V10),(cl_index)(&V9)};
          ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
          ihs.lex_env = _ecl_debug_env;
          V5 = ecl_length(V1);
          V8 = ecl_length(V2);
L12:;
          if (!((V4)>=(V5))) { goto L18; }
          goto L13;
L18:;
          V3 = ecl_aref1(V1,V4);
          {
          cl_object V11;
          V11 = ecl_make_integer((V4)+1);
          {
          static const struct ecl_var_debug_info _ecl_descriptors[]={
          {"#:%CHECKED-VALUE",_ecl_object_loc}};
          const cl_index _ecl_debug_info_raw[]={
          (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V11)};
          ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
          ihs.lex_env = _ecl_debug_env;
          {
          bool V12;
          V12 = ECL_FIXNUMP(V11);
          if (ecl_unlikely(!(V12)))
         FEwrong_type_argument(ECL_SYM("FIXNUM",372),V11);
          }
          V4 = ecl_fixnum(V11);
          }
          ihs.lex_env = _ecl_debug_env;
          }
          if (!((V7)>=(V8))) { goto L26; }
          goto L13;
L26:;
          V6 = ecl_aref1(V2,V7);
          {
          cl_object V11;
          V11 = ecl_make_integer((V7)+1);
          {
          static const struct ecl_var_debug_info _ecl_descriptors[]={
          {"#:%CHECKED-VALUE",_ecl_object_loc}};
          const cl_index _ecl_debug_info_raw[]={
          (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V11)};
          ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
          ihs.lex_env = _ecl_debug_env;
          {
          bool V12;
          V12 = ECL_FIXNUMP(V11);
          if (ecl_unlikely(!(V12)))
         FEwrong_type_argument(ECL_SYM("FIXNUM",372),V11);
          }
          V7 = ecl_fixnum(V11);
          }
          ihs.lex_env = _ecl_debug_env;
          }
          T2 = V10;
          T3 = ecl_times(V3,V6);
          V10 = ecl_list1(T3);
          cl_rplacd(T2, V10);
          goto L12;
L13:;
          T1 = ecl_cdr(V9);
          goto L3;
         }
         ihs.lex_env = _ecl_debug_env;
        }
       }
       ihs.lex_env = _ecl_debug_env;
      }
     }
     ihs.lex_env = _ecl_debug_env;
    }
L3:;
    value0 = cl_make_array(3, T0, ECL_SYM("INITIAL-CONTENTS",1252), T1);
    ecl_ihs_pop(cl_env_copy);
    return value0;
   }
  }
 }
}
/*	function definition for VECTOR-SUM                            */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L2vector_sum(cl_object V1, cl_object V2)
{
 cl_object T0, T1, T2, T3;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::X",_ecl_object_loc}
   ,{"COMMON-LISP-USER::Y",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[1],_ecl_debug_env);
TTL:
   T0 = cl_array_total_size(V1);
   {
    cl_object V3;                                 /*  I               */
    cl_object V4;
    cl_fixnum V5;
    cl_fixnum V6;
    V3 = ECL_NIL;
    {
     if (ecl_unlikely(!ECL_VECTORP(V1))) FEtype_error_vector(V1);
     V4 = V1;
    }
    V5 = 0;
    V6 = 0;
    {
     static const struct ecl_var_debug_info _ecl_descriptors[]={
     {"#:LOOP-ACROSS-LIST10",_ecl_fixnum_loc}
     ,{"#:LOOP-ACROSS-INDEX-9",_ecl_fixnum_loc}
     ,{"#:LOOP-ACROSS-VECTOR-8",_ecl_object_loc}
     ,{"COMMON-LISP-USER::I",_ecl_object_loc}};
     const cl_index _ecl_debug_info_raw[]={
     (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V6),(cl_index)(&V5),(cl_index)(&V4),(cl_index)(&V3)};
     ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,6,,);
     ihs.lex_env = _ecl_debug_env;
     {
      cl_object V7;                               /*  J               */
      cl_object V8;
      cl_fixnum V9;
      cl_fixnum V10;
      V7 = ECL_NIL;
      {
       if (ecl_unlikely(!ECL_VECTORP(V2))) FEtype_error_vector(V2);
       V8 = V2;
      }
      V9 = 0;
      V10 = 0;
      {
       static const struct ecl_var_debug_info _ecl_descriptors[]={
       {"#:LOOP-ACROSS-LIST13",_ecl_fixnum_loc}
       ,{"#:LOOP-ACROSS-INDEX-12",_ecl_fixnum_loc}
       ,{"#:LOOP-ACROSS-VECTOR-11",_ecl_object_loc}
       ,{"COMMON-LISP-USER::J",_ecl_object_loc}};
       const cl_index _ecl_debug_info_raw[]={
       (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V10),(cl_index)(&V9),(cl_index)(&V8),(cl_index)(&V7)};
       ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,6,,);
       ihs.lex_env = _ecl_debug_env;
       {
        cl_object V11;
        cl_object V12;
        V11 = ecl_list1(ECL_NIL);
        V12 = V11;
        {
         static const struct ecl_var_debug_info _ecl_descriptors[]={
         {"#:LOOP-LIST-TAIL15",_ecl_object_loc}
         ,{"#:LOOP-LIST-HEAD14",_ecl_object_loc}};
         const cl_index _ecl_debug_info_raw[]={
         (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V12),(cl_index)(&V11)};
         ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
         ihs.lex_env = _ecl_debug_env;
         V6 = ecl_length(V4);
         V10 = ecl_length(V8);
L14:;
         if (!((V5)>=(V6))) { goto L20; }
         goto L15;
L20:;
         V3 = ecl_aref1(V4,V5);
         {
          cl_object V13;
          V13 = ecl_make_integer((V5)+1);
          {
          static const struct ecl_var_debug_info _ecl_descriptors[]={
          {"#:%CHECKED-VALUE",_ecl_object_loc}};
          const cl_index _ecl_debug_info_raw[]={
          (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V13)};
          ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
          ihs.lex_env = _ecl_debug_env;
          {
          bool V14;
          V14 = ECL_FIXNUMP(V13);
          if (ecl_unlikely(!(V14)))
         FEwrong_type_argument(ECL_SYM("FIXNUM",372),V13);
          }
          V5 = ecl_fixnum(V13);
          }
          ihs.lex_env = _ecl_debug_env;
         }
         if (!((V9)>=(V10))) { goto L28; }
         goto L15;
L28:;
         V7 = ecl_aref1(V8,V9);
         {
          cl_object V13;
          V13 = ecl_make_integer((V9)+1);
          {
          static const struct ecl_var_debug_info _ecl_descriptors[]={
          {"#:%CHECKED-VALUE",_ecl_object_loc}};
          const cl_index _ecl_debug_info_raw[]={
          (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V13)};
          ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
          ihs.lex_env = _ecl_debug_env;
          {
          bool V14;
          V14 = ECL_FIXNUMP(V13);
          if (ecl_unlikely(!(V14)))
         FEwrong_type_argument(ECL_SYM("FIXNUM",372),V13);
          }
          V9 = ecl_fixnum(V13);
          }
          ihs.lex_env = _ecl_debug_env;
         }
         T2 = V12;
         T3 = ecl_plus(V3,V7);
         V12 = ecl_list1(T3);
         cl_rplacd(T2, V12);
         goto L14;
L15:;
         T1 = ecl_cdr(V11);
         goto L1;
        }
        ihs.lex_env = _ecl_debug_env;
       }
      }
      ihs.lex_env = _ecl_debug_env;
     }
    }
    ihs.lex_env = _ecl_debug_env;
   }
L1:;
   value0 = cl_make_array(3, T0, ECL_SYM("INITIAL-CONTENTS",1252), T1);
   ecl_ihs_pop(cl_env_copy);
   return value0;
  }
 }
}
/*	function definition for 5PT-STENCIL                           */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L35pt_stencil(cl_object V1, cl_object V2, cl_object V3, cl_object V4)
{
 cl_object T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::I",_ecl_object_loc}
   ,{"COMMON-LISP-USER::ORDER",_ecl_object_loc}
   ,{"COMMON-LISP-USER::X",_ecl_object_loc}
   ,{"COMMON-LISP-USER::H",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2),(cl_index)(&V3),(cl_index)(&V4)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,6,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[3],_ecl_debug_env);
TTL:
   {
    bool V5;
    V5 = ECL_FIXNUMP(V1);
    if (ecl_unlikely(!(V5)))
         FEwrong_type_argument(ECL_SYM("FIXNUM",372),V1);
   }
   {
    bool V5;
    V5 = ECL_FIXNUMP(V2);
    if (ecl_unlikely(!(V5)))
         FEwrong_type_argument(ECL_SYM("FIXNUM",372),V2);
   }
   T0 = cl_typep(2, V3, VV[2]);
   if (ecl_unlikely(!((T0)!=ECL_NIL)))
         FEwrong_type_argument(VV[2],V3);
   {
    bool V5;
    V5 = floatp(V4);
    if (ecl_unlikely(!(V5)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V4);
   }
   {
    {
     cl_fixnum V5;                                /*  LEN             */
     cl_object V6;                                /*  XI-2            */
     cl_object V7;                                /*  XI-1            */
     cl_object V8;                                /*  XI              */
     cl_object V9;                                /*  XI+1            */
     cl_object V10;                               /*  XI+2            */
     V5 = ecl_fixnum(cl_array_total_size(V3));
     T0 = ecl_minus(V1,ecl_make_fixnum(2));
     if (!(ecl_number_compare(ecl_make_fixnum(0),T0)>0)) { goto L7; }
     T0 = ecl_minus(ecl_make_fixnum(V5),ecl_make_fixnum(2));
     T1 = ecl_plus(V1,T0);
     V6 = ecl_aref1(V3,ecl_to_size(T1));
     goto L6;
L7:;
     T0 = ecl_minus(V1,ecl_make_fixnum(2));
     V6 = ecl_aref1(V3,ecl_to_size(T0));
L6:;
     if (!((0)==(ecl_to_fixnum(V1)))) { goto L10; }
     T0 = ecl_minus(ecl_make_fixnum(V5),ecl_make_fixnum(1));
     V7 = ecl_aref1(V3,ecl_to_size(T0));
     goto L9;
L10:;
     T0 = ecl_minus(V1,ecl_make_fixnum(1));
     V7 = ecl_aref1(V3,ecl_to_size(T0));
L9:;
     V8 = ecl_aref1(V3,ecl_to_fixnum(V1));
     T0 = ecl_minus(ecl_make_fixnum(V5),ecl_make_fixnum(1));
     if (!(ecl_number_compare(T0,V1)>=0)) { goto L14; }
     V9 = ecl_aref1(V3,0);
     goto L13;
L14:;
     T0 = ecl_plus(V1,ecl_make_fixnum(1));
     V9 = ecl_aref1(V3,ecl_to_size(T0));
L13:;
     T0 = ecl_minus(ecl_make_fixnum(V5),V1);
     if (!(ecl_number_compare(ecl_make_fixnum(2),T0)>=0)) { goto L17; }
     T0 = ecl_minus(ecl_make_fixnum(V5),V1);
     T1 = ecl_minus(T0,ecl_make_fixnum(1));
     V10 = ecl_aref1(V3,ecl_to_size(T1));
     goto L16;
L17:;
     T0 = ecl_plus(V1,ecl_make_fixnum(2));
     V10 = ecl_aref1(V3,ecl_to_size(T0));
L16:;
     {
      static const struct ecl_var_debug_info _ecl_descriptors[]={
      {"COMMON-LISP-USER::XI+2",_ecl_object_loc}
      ,{"COMMON-LISP-USER::XI+1",_ecl_object_loc}
      ,{"COMMON-LISP-USER::XI",_ecl_object_loc}
      ,{"COMMON-LISP-USER::XI-1",_ecl_object_loc}
      ,{"COMMON-LISP-USER::XI-2",_ecl_object_loc}
      ,{"COMMON-LISP-USER::LEN",_ecl_fixnum_loc}};
      const cl_index _ecl_debug_info_raw[]={
      (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V10),(cl_index)(&V9),(cl_index)(&V8),(cl_index)(&V7),(cl_index)(&V6),(cl_index)(&V5)};
      ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,8,,);
      ihs.lex_env = _ecl_debug_env;
      if (!((ecl_to_fixnum(V2))==(1))) { goto L19; }
      T0 = ecl_negate(V6);
      T1 = ecl_negate(V7);
      T2 = ecl_plus(T0,T1);
      T3 = ecl_plus(T2,V9);
      T4 = ecl_plus(T3,V10);
      T5 = ecl_times(ecl_make_fixnum(6),V4);
      T0 = ecl_negate(V6);
      T1 = ecl_times(ecl_make_fixnum(16),V7);
      T2 = ecl_plus(T0,T1);
      T3 = ecl_times(ecl_make_fixnum(-30),V8);
      T4 = ecl_plus(T2,T3);
      T5 = ecl_times(ecl_make_fixnum(16),V9);
      T6 = ecl_plus(T4,T5);
      T7 = ecl_negate(V10);
      T8 = ecl_plus(T6,T7);
      T9 = ecl_expt(V4,ecl_make_fixnum(2));
      T10 = ecl_times(ecl_make_fixnum(12),T9);
      T0 = ecl_times(ecl_make_fixnum(-2),V7);
      T1 = ecl_plus(V6,T0);
      T2 = ecl_times(ecl_make_fixnum(2),V9);
      T3 = ecl_plus(T1,T2);
      T4 = ecl_negate(V10);
      T5 = ecl_plus(T3,T4);
      T6 = ecl_expt(V4,ecl_make_fixnum(3));
      T7 = ecl_times(ecl_make_fixnum(2),T6);
      value0 = V8;
      cl_env_copy->nvalues = 1;
      ecl_ihs_pop(cl_env_copy);
      return value0;
L19:;
      value0 = ECL_NIL;
      cl_env_copy->nvalues = 1;
      ecl_ihs_pop(cl_env_copy);
      return value0;
     }
     ihs.lex_env = _ecl_debug_env;
    }
   }
  }
 }
}
/*	function definition for RK4                                   */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L4rk4(cl_object V1, cl_object V2, cl_object V3, cl_object V4, cl_object V5)
{
 cl_object T0, T1, T2, T3, T4;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::F",_ecl_object_loc}
   ,{"COMMON-LISP-USER::I",_ecl_object_loc}
   ,{"COMMON-LISP-USER::DT",_ecl_object_loc}
   ,{"COMMON-LISP-USER::X",_ecl_object_loc}
   ,{"COMMON-LISP:TIME",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2),(cl_index)(&V3),(cl_index)(&V4),(cl_index)(&V5)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,7,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[6],_ecl_debug_env);
TTL:
   T0 = cl_functionp(V1);
   if (ecl_unlikely(!((T0)!=ECL_NIL)))
         FEwrong_type_argument(ECL_SYM("FUNCTION",396),V1);
   {
    bool V6;
    V6 = ECL_FIXNUMP(V2);
    if (ecl_unlikely(!(V6)))
         FEwrong_type_argument(ECL_SYM("FIXNUM",372),V2);
   }
   {
    bool V6;
    V6 = floatp(V3);
    if (ecl_unlikely(!(V6)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V3);
   }
   {
    bool V6;
    V6 = floatp(V4);
    if (ecl_unlikely(!(V6)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V4);
   }
   {
    bool V6;
    V6 = floatp(V5);
    if (ecl_unlikely(!(V6)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V5);
   }
   {
    {
     cl_object V6;                                /*  K_1             */
     cl_object V7;                                /*  K_2             */
     cl_object V8;                                /*  K_3             */
     cl_object V9;                                /*  K_4             */
     V6 = ECL_NIL;
     V7 = ECL_NIL;
     V8 = ECL_NIL;
     V9 = ECL_NIL;
     {
      static const struct ecl_var_debug_info _ecl_descriptors[]={
      {"COMMON-LISP-USER::K_4",_ecl_object_loc}
      ,{"COMMON-LISP-USER::K_3",_ecl_object_loc}
      ,{"COMMON-LISP-USER::K_2",_ecl_object_loc}
      ,{"COMMON-LISP-USER::K_1",_ecl_object_loc}};
      const cl_index _ecl_debug_info_raw[]={
      (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V9),(cl_index)(&V8),(cl_index)(&V7),(cl_index)(&V6)};
      ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,6,,);
      ihs.lex_env = _ecl_debug_env;
      V6 = ecl_function_dispatch(cl_env_copy,V1)(2, V5, V4);
      T0 = ecl_times(VV[4],V3);
      T1 = ecl_plus(V5,T0);
      T2 = ecl_times(VV[4],V3);
      T3 = ecl_times(T2,V6);
      T4 = ecl_plus(V4,T3);
      V7 = ecl_function_dispatch(cl_env_copy,V1)(2, T1, T4);
      T0 = ecl_times(VV[4],V3);
      T1 = ecl_plus(V5,T0);
      T2 = ecl_times(VV[4],V3);
      T3 = ecl_times(T2,V7);
      T4 = ecl_plus(V4,T3);
      V8 = ecl_function_dispatch(cl_env_copy,V1)(2, T1, T4);
      T0 = ecl_plus(V5,V3);
      T1 = ecl_times(V3,V8);
      T2 = ecl_plus(V4,T1);
      V9 = ecl_function_dispatch(cl_env_copy,V1)(2, T0, T2);
      T0 = ecl_times(V3,VV[5]);
      T1 = ecl_plus(V6,V7);
      T2 = ecl_plus(T1,V8);
      T3 = ecl_plus(T2,V9);
      T4 = ecl_times(T0,T3);
      value0 = ecl_plus(V4,T4);
      cl_env_copy->nvalues = 1;
      ecl_ihs_pop(cl_env_copy);
      return value0;
     }
     ihs.lex_env = _ecl_debug_env;
    }
   }
  }
 }
}
/*	function definition for RK4-SEQ                               */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L9rk4_seq(cl_object V1, cl_object V2, cl_object V3)
{
 cl_object T0, T1, T2, T3;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 cl_object env0;
 cl_object CLV0
 ;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  env0 = ECL_NIL;
  CLV0 = env0 = CONS(V2,env0);                    /*  DT              */
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::F",_ecl_object_loc}
   ,{"COMMON-LISP-USER::X",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V3)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[7],_ecl_debug_env);
   {
    cl_object V4;                                 /*  U               */
    cl_object V5;                                 /*  K_1             */
    cl_object V6;                                 /*  K_2             */
    cl_object V7;                                 /*  K_3             */
    cl_object V8;                                 /*  K_4             */
    V4 = ecl_copy_seq(V3);
    V5 = ecl_function_dispatch(cl_env_copy,V1)(1, V4);
    {cl_object V9;
     V9 = ECL_NIL;
     V9 = ecl_make_cclosure_va((cl_objectfn)LC5__g20,env0,Cblock);
     T0 = V9;
    }
    T1 = cl_map(3, ECL_SYM("VECTOR",898), T0, V5);
    T2 = ecl_function_dispatch(cl_env_copy,VV[1])(2, V4, T1) /*  VECTOR-SUM */;
    V6 = ecl_function_dispatch(cl_env_copy,V1)(1, T2);
    {cl_object V9;
     V9 = ECL_NIL;
     V9 = ecl_make_cclosure_va((cl_objectfn)LC6__g21,env0,Cblock);
     T0 = V9;
    }
    T1 = cl_map(3, ECL_SYM("VECTOR",898), T0, V6);
    T2 = ecl_function_dispatch(cl_env_copy,VV[1])(2, V4, T1) /*  VECTOR-SUM */;
    V7 = ecl_function_dispatch(cl_env_copy,V1)(1, T2);
    {cl_object V9;
     V9 = ECL_NIL;
     V9 = ecl_make_cclosure_va((cl_objectfn)LC7__g22,env0,Cblock);
     T0 = V9;
    }
    T1 = cl_map(3, ECL_SYM("VECTOR",898), T0, V7);
    T2 = ecl_function_dispatch(cl_env_copy,VV[1])(2, V4, T1) /*  VECTOR-SUM */;
    V8 = ecl_function_dispatch(cl_env_copy,V1)(1, T2);
    {
     static const struct ecl_var_debug_info _ecl_descriptors[]={
     {"COMMON-LISP-USER::K_4",_ecl_object_loc}
     ,{"COMMON-LISP-USER::K_3",_ecl_object_loc}
     ,{"COMMON-LISP-USER::K_2",_ecl_object_loc}
     ,{"COMMON-LISP-USER::K_1",_ecl_object_loc}
     ,{"COMMON-LISP-USER::U",_ecl_object_loc}};
     const cl_index _ecl_debug_info_raw[]={
     (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V8),(cl_index)(&V7),(cl_index)(&V6),(cl_index)(&V5),(cl_index)(&V4)};
     ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,7,,);
     ihs.lex_env = _ecl_debug_env;
     {cl_object V9;
      V9 = ECL_NIL;
      V9 = ecl_make_cclosure_va((cl_objectfn)LC8__g23,env0,Cblock);
      T0 = V9;
     }
     T1 = ecl_function_dispatch(cl_env_copy,VV[1])(2, V5, V6) /*  VECTOR-SUM */;
     T2 = ecl_function_dispatch(cl_env_copy,VV[1])(2, V7, V8) /*  VECTOR-SUM */;
     T3 = ecl_function_dispatch(cl_env_copy,VV[1])(2, T1, T2) /*  VECTOR-SUM */;
     value0 = cl_map(3, ECL_SYM("VECTOR",898), T0, T3);
     ecl_ihs_pop(cl_env_copy);
     return value0;
    }
    ihs.lex_env = _ecl_debug_env;
   }
  }
 }
}
/*	closure G20                                                   */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object LC5__g20(cl_narg narg, cl_object V1, ...)
{
 cl_object T0;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 cl_object CLV0
 ;
 const cl_env_ptr cl_env_copy = ecl_process_env();cl_object env0 = cl_env_copy->function->cclosure.env;
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 /* Scanning closure data ... */
 CLV0 = env0;                                     /*  DT              */
 { /* ... closure scanning finished */
 if (ecl_unlikely(narg!=1)) FEwrong_num_arguments_anonym();
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::X",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[16],_ecl_debug_env);
TTL:
   T0 = ecl_times(VV[4],ECL_CONS_CAR(CLV0));
   value0 = ecl_times(T0,V1);
   cl_env_copy->nvalues = 1;
   ecl_ihs_pop(cl_env_copy);
   return value0;
  }
 }
 }
}
/*	closure G21                                                   */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object LC6__g21(cl_narg narg, cl_object V1, ...)
{
 cl_object T0;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 cl_object CLV0
 ;
 const cl_env_ptr cl_env_copy = ecl_process_env();cl_object env0 = cl_env_copy->function->cclosure.env;
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 /* Scanning closure data ... */
 CLV0 = env0;                                     /*  DT              */
 { /* ... closure scanning finished */
 if (ecl_unlikely(narg!=1)) FEwrong_num_arguments_anonym();
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::X",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[17],_ecl_debug_env);
TTL:
   T0 = ecl_times(VV[4],ECL_CONS_CAR(CLV0));
   value0 = ecl_times(T0,V1);
   cl_env_copy->nvalues = 1;
   ecl_ihs_pop(cl_env_copy);
   return value0;
  }
 }
 }
}
/*	closure G22                                                   */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object LC7__g22(cl_narg narg, cl_object V1, ...)
{
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 cl_object CLV0
 ;
 const cl_env_ptr cl_env_copy = ecl_process_env();cl_object env0 = cl_env_copy->function->cclosure.env;
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 /* Scanning closure data ... */
 CLV0 = env0;                                     /*  DT              */
 { /* ... closure scanning finished */
 if (ecl_unlikely(narg!=1)) FEwrong_num_arguments_anonym();
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::X",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[18],_ecl_debug_env);
TTL:
   value0 = ecl_times(ECL_CONS_CAR(CLV0),V1);
   cl_env_copy->nvalues = 1;
   ecl_ihs_pop(cl_env_copy);
   return value0;
  }
 }
 }
}
/*	closure G23                                                   */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object LC8__g23(cl_narg narg, cl_object V1, ...)
{
 cl_object T0;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 cl_object CLV0
 ;
 const cl_env_ptr cl_env_copy = ecl_process_env();cl_object env0 = cl_env_copy->function->cclosure.env;
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 /* Scanning closure data ... */
 CLV0 = env0;                                     /*  DT              */
 { /* ... closure scanning finished */
 if (ecl_unlikely(narg!=1)) FEwrong_num_arguments_anonym();
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::X",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[19],_ecl_debug_env);
TTL:
   T0 = ecl_times(ECL_CONS_CAR(CLV0),VV[5]);
   value0 = ecl_times(T0,V1);
   cl_env_copy->nvalues = 1;
   ecl_ihs_pop(cl_env_copy);
   return value0;
  }
 }
 }
}
/*	function definition for NEWTON-FXN                            */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L12newton_fxn(cl_object V1, cl_object V2, cl_object V3, cl_object V4)
{
 cl_object T0;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 volatile cl_object lex0[3];
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::X_0",_ecl_object_loc}
   ,{"COMMON-LISP-USER::F",_ecl_object_loc}
   ,{"COMMON-LISP-USER::DF-DX",_ecl_object_loc}
   ,{"COMMON-LISP-USER::ERR",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2),(cl_index)(&V3),(cl_index)(&V4)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,6,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[8],_ecl_debug_env);
TTL:
   {
    bool V5;
    V5 = floatp(V1);
    if (ecl_unlikely(!(V5)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V1);
   }
   T0 = cl_functionp(V2);
   if (ecl_unlikely(!((T0)!=ECL_NIL)))
         FEwrong_type_argument(ECL_SYM("FUNCTION",396),V2);
   T0 = cl_functionp(V3);
   if (ecl_unlikely(!((T0)!=ECL_NIL)))
         FEwrong_type_argument(ECL_SYM("FUNCTION",396),V3);
   {
    bool V5;
    V5 = floatp(V4);
    if (ecl_unlikely(!(V5)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V4);
   }
   lex0[0] = V2;                                  /*  F               */
   lex0[1] = V3;                                  /*  DF-DX           */
   lex0[2] = V4;                                  /*  ERR             */
   {
    T0 = LC10next(lex0, V1);
    value0 = LC11check(lex0, V1, T0);
    ecl_ihs_pop(cl_env_copy);
    return value0;
   }
  }
 }
}
/*	local function CHECK                                          */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object LC11check(volatile cl_object *lex0, cl_object V1, cl_object V2)
{
 cl_object T0, T1;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::X",_ecl_object_loc}
   ,{"COMMON-LISP-USER::Y",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[21],_ecl_debug_env);
TTL:
   T0 = ecl_minus(V1,V2);
   T1 = cl_abs(T0);
   if (!(ecl_number_compare(lex0[2],T1)>0)) { goto L1; }
   value0 = V2;
   cl_env_copy->nvalues = 1;
   ecl_ihs_pop(cl_env_copy);
   return value0;
L1:;
   V1 = V2;
   V2 = LC10next(lex0, V2);
   goto TTL;
  }
 }
}
/*	local function NEXT                                           */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object LC10next(volatile cl_object *lex0, cl_object V1)
{
 cl_object T0, T1, T2;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::X",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[22],_ecl_debug_env);
TTL:
   T0 = ecl_function_dispatch(cl_env_copy,lex0[0])(1, V1);
   T1 = ecl_function_dispatch(cl_env_copy,lex0[1])(1, V1);
   T2 = ecl_divide(T0,T1);
   value0 = ecl_minus(V1,T2);
   cl_env_copy->nvalues = 1;
   ecl_ihs_pop(cl_env_copy);
   return value0;
  }
 }
}

#include "calculus.data"
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
 flag->cblock.source = make_constant_base_string("/home/tucker/Repo/my-code/lisp/calculus.lisp");
 return;}
 #ifdef ECL_DYNAMIC_VV
 VV = Cblock->cblock.data;
 #endif
 Cblock->cblock.data_text = "@EcLtAg:init_fas_CODE@";
 VVtemp = Cblock->cblock.temp_data;
 ECL_DEFINE_SETF_FUNCTIONS
 ecl_function_dispatch(cl_env_copy,VV[9])(10, VVtemp[0], ECL_NIL, ECL_NIL, VVtemp[1], ECL_NIL, ECL_NIL, VVtemp[2], ECL_NIL, ECL_NIL, ECL_NIL) /*  DODEFPACKAGE */;
 ecl_cmp_defun(VV[10]);                           /*  DOT-PROD        */
 ecl_cmp_defun(VV[11]);                           /*  VECTOR-SUM      */
 ecl_cmp_defun(VV[12]);                           /*  5PT-STENCIL     */
 ecl_cmp_defun(VV[13]);                           /*  RK4             */
 ecl_cmp_defun(VV[14]);                           /*  RK4-SEQ         */
 ecl_function_dispatch(cl_env_copy,VV[15])(3, VV[7], ECL_SYM("FUNCTION",396), VVtemp[3]) /*  SET-DOCUMENTATION */;
 ecl_cmp_defun(VV[20]);                           /*  NEWTON-FXN      */
}
