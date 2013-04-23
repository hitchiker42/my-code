/*	Compiler: ECL 12.12.1                                         */
/*	Date: 2013/4/22 21:06 (yyyy/mm/dd)                            */
/*	Machine: Linux 3.8.5-201.fc18.x86_64 x86_64                   */
/*	Source: kdv.lisp                                              */
#include <ecl/ecl-cmp.h>
#include "kdv.eclh"
/*	local function ARRAY-LEN                                      */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object LC1array_len(cl_object V1, cl_object V2)
{
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"#:G4",_ecl_object_loc}
   ,{"#:G3",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[1],_ecl_debug_env);
TTL:
   {
    cl_object V3;
    cl_object V4;                                 /*  Y               */
    V3 = ecl_cdr(V1);
    if (!(V3==ECL_NIL)) { goto L3; }
    ecl_function_dispatch(cl_env_copy,VV[15])(1, V1) /*  DM-TOO-FEW-ARGUMENTS */;
L3:;
    {
     cl_object V5;
     V5 = ecl_car(V3);
     {
      static const struct ecl_var_debug_info _ecl_descriptors[]={
      {"#:G6",_ecl_object_loc}};
      const cl_index _ecl_debug_info_raw[]={
      (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V5)};
      ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
      ihs.lex_env = _ecl_debug_env;
      V3 = ecl_cdr(V3);
      V4 = V5;
     }
     ihs.lex_env = _ecl_debug_env;
    }
    {
     static const struct ecl_var_debug_info _ecl_descriptors[]={
     {"COMMON-LISP-USER::Y",_ecl_object_loc}
     ,{"#:G5",_ecl_object_loc}};
     const cl_index _ecl_debug_info_raw[]={
     (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V4),(cl_index)(&V3)};
     ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
     ihs.lex_env = _ecl_debug_env;
     if (Null(V3)) { goto L8; }
     ecl_function_dispatch(cl_env_copy,VV[16])(1, V1) /*  DM-TOO-MANY-ARGUMENTS */;
L8:;
     value0 = cl_list(2, ECL_SYM("ARRAY-TOTAL-SIZE",107), V4);
     ecl_ihs_pop(cl_env_copy);
     return value0;
    }
    ihs.lex_env = _ecl_debug_env;
   }
  }
 }
}
/*	function definition for U-INIT                                */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L2u_init(cl_object V1)
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
   {"COMMON-LISP-USER::Y",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[2],_ecl_debug_env);
TTL:
   {
    bool V2;
    V2 = floatp(V1);
    if (ecl_unlikely(!(V2)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V1);
   }
   {
    T0 = ecl_cosh(V1);
    T1 = ecl_divide(ecl_make_fixnum(1),T0);
    T2 = ecl_expt(T1,ecl_make_fixnum(2));
    value0 = ecl_times(ecl_make_fixnum(-12),T2);
    cl_env_copy->nvalues = 1;
    ecl_ihs_pop(cl_env_copy);
    return value0;
   }
  }
 }
}
/*	function definition for U-DISCRETE                            */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L3u_discrete(cl_object V1, cl_object V2)
{
 cl_object T0, T1, T2, T3, T4, T5, T6, T7;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::U",_ecl_object_loc}
   ,{"COMMON-LISP-USER::H",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[4],_ecl_debug_env);
TTL:
   T0 = cl_typep(2, V1, VV[3]);
   if (ecl_unlikely(!((T0)!=ECL_NIL)))
         FEwrong_type_argument(VV[3],V1);
   {
    bool V3;
    V3 = floatp(V2);
    if (ecl_unlikely(!(V3)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V2);
   }
   {
    {
     cl_fixnum V3;                                /*  LEN             */
     V3 = ecl_fixnum(cl_array_total_size(V1));
     {
      static const struct ecl_var_debug_info _ecl_descriptors[]={
      {"COMMON-LISP-USER::LEN",_ecl_fixnum_loc}};
      const cl_index _ecl_debug_info_raw[]={
      (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V3)};
      ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
      ihs.lex_env = _ecl_debug_env;
      {
       cl_object V4;                              /*  LET26           */
       cl_object V5;
       cl_object V6;                              /*  I               */
       V4 = ecl_minus(ecl_make_fixnum(V3),ecl_make_fixnum(1));
       V5 = V4;
       V6 = ecl_make_fixnum(0);
       {
        static const struct ecl_var_debug_info _ecl_descriptors[]={
        {"COMMON-LISP-USER::I",_ecl_object_loc}
        ,{"#:LOOP-LIMIT7",_ecl_object_loc}
        ,{"COMMON-LISP-USER::LET26",_ecl_object_loc}};
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
          {"#:LOOP-LIST-TAIL9",_ecl_object_loc}
          ,{"#:LOOP-LIST-HEAD8",_ecl_object_loc}};
          const cl_index _ecl_debug_info_raw[]={
          (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V8),(cl_index)(&V7)};
          ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
          ihs.lex_env = _ecl_debug_env;
L10:;
          if (!(ecl_number_compare(V6,V5)>0)) { goto L12; }
          goto L11;
L12:;
          T1 = V8;
          T2 = ecl_function_dispatch(cl_env_copy,VV[20])(4, V6, ecl_make_fixnum(3), V1, V2) /*  5PT-STENCIL */;
          T3 = ecl_aref1(V1,ecl_to_size(V6));
          T4 = ecl_times(ecl_make_fixnum(-6),T3);
          T5 = ecl_function_dispatch(cl_env_copy,VV[20])(4, V6, ecl_make_fixnum(1), V1, V2) /*  5PT-STENCIL */;
          T6 = ecl_times(T4,T5);
          T7 = ecl_plus(T2,T6);
          V8 = ecl_list1(T7);
          cl_rplacd(T1, V8);
          V6 = ecl_one_plus(V6);
          goto L10;
L11:;
          T0 = ecl_cdr(V7);
          goto L4;
         }
         ihs.lex_env = _ecl_debug_env;
        }
       }
       ihs.lex_env = _ecl_debug_env;
      }
L4:;
      value0 = cl_make_array(3, ecl_make_fixnum(V3), ECL_SYM("INITIAL-CONTENTS",1252), T0);
      ecl_ihs_pop(cl_env_copy);
      return value0;
     }
     ihs.lex_env = _ecl_debug_env;
    }
   }
  }
 }
}
/*	function definition for U-DISCRETE-I                          */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L4u_discrete_i(cl_object V1, cl_object V2, cl_object V3)
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
   {"COMMON-LISP-USER::U",_ecl_object_loc}
   ,{"COMMON-LISP-USER::H",_ecl_object_loc}
   ,{"COMMON-LISP-USER::I",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2),(cl_index)(&V3)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,5,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[5],_ecl_debug_env);
TTL:
   T0 = cl_typep(2, V1, VV[3]);
   if (ecl_unlikely(!((T0)!=ECL_NIL)))
         FEwrong_type_argument(VV[3],V1);
   {
    bool V4;
    V4 = floatp(V2);
    if (ecl_unlikely(!(V4)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V2);
   }
   {
    bool V4;
    V4 = ECL_FIXNUMP(V3);
    if (ecl_unlikely(!(V4)))
         FEwrong_type_argument(ECL_SYM("FIXNUM",372),V3);
   }
   {
    T0 = ecl_function_dispatch(cl_env_copy,VV[20])(4, V3, ecl_make_fixnum(3), V1, V2) /*  5PT-STENCIL */;
    T1 = ecl_aref1(V1,ecl_to_fixnum(V3));
    T2 = ecl_times(ecl_make_fixnum(-6),T1);
    T3 = ecl_function_dispatch(cl_env_copy,VV[20])(4, V3, ecl_make_fixnum(1), V1, V2) /*  5PT-STENCIL */;
    T4 = ecl_times(T2,T3);
    value0 = ecl_plus(T0,T4);
    cl_env_copy->nvalues = 1;
    ecl_ihs_pop(cl_env_copy);
    return value0;
   }
  }
 }
}
/*	function definition for UPDATE                                */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L6update(cl_object V1, cl_object V2, cl_object V3, cl_object V4)
{
 cl_object T0;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 cl_object env0;
 cl_object CLV0
 ;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::U",_ecl_object_loc}
   ,{"COMMON-LISP:TIME",_ecl_object_loc}
   ,{"COMMON-LISP-USER::DT",_ecl_object_loc}
   ,{"COMMON-LISP-USER::DX",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1),(cl_index)(&V2),(cl_index)(&V3),(cl_index)(&V4)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,6,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[6],_ecl_debug_env);
TTL:
   T0 = cl_typep(2, V1, VV[3]);
   if (ecl_unlikely(!((T0)!=ECL_NIL)))
         FEwrong_type_argument(VV[3],V1);
   {
    bool V5;
    V5 = floatp(V2);
    if (ecl_unlikely(!(V5)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V2);
   }
   {
    bool V5;
    V5 = floatp(V3);
    if (ecl_unlikely(!(V5)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V3);
   }
   {
    bool V5;
    V5 = floatp(V4);
    if (ecl_unlikely(!(V5)))
         FEwrong_type_argument(ECL_SYM("FLOAT",374),V4);
   }
   {
    cl_object env1 = env0;
    env1 = ECL_NIL;
    CLV0 = env1 = CONS(V4,env1);                  /*  DX              */
    {
     {cl_object V5;
      V5 = ECL_NIL;
      V5 = ecl_make_cclosure_va((cl_objectfn)LC5ustep,env1,Cblock);
      value0 = ecl_function_dispatch(cl_env_copy,VV[23])(3, V5, V3, V1) /*  RK4-SEQ */;
      ecl_ihs_pop(cl_env_copy);
      return value0;
     }
    }
   }
  }
 }
}
/*	closure USTEP                                                 */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object LC5ustep(cl_narg narg, cl_object V1, ...)
{
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 cl_object CLV0
 ;
 const cl_env_ptr cl_env_copy = ecl_process_env();cl_object env0 = cl_env_copy->function->cclosure.env;
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 /* Scanning closure data ... */
 CLV0 = env0;                                     /*  DX              */
 { /* ... closure scanning finished */
 if (ecl_unlikely(narg!=1)) FEwrong_num_arguments_anonym();
 {
  {
   static const struct ecl_var_debug_info _ecl_descriptors[]={
   {"COMMON-LISP-USER::X",_ecl_object_loc}};
   const cl_index _ecl_debug_info_raw[]={
   (cl_index)(ECL_NIL),(cl_index)(_ecl_descriptors),(cl_index)(&V1)};
   ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
   ecl_ihs_push(cl_env_copy,&ihs,VV[24],_ecl_debug_env);
TTL:
   value0 = ecl_function_dispatch(cl_env_copy,VV[4])(2, V1, ECL_CONS_CAR(CLV0)) /*  U-DISCRETE */;
   ecl_ihs_pop(cl_env_copy);
   return value0;
  }
 }
 }
}
/*	function definition for MAIN                                  */
/*	optimize speed 3, debug 3, space 0, safety 2                  */
static cl_object L7main()
{
 cl_object T0, T1, T2, T3, T4, T5;
 struct ecl_ihs_frame ihs;
 const cl_object _ecl_debug_env = ECL_NIL;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 volatile cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  {
   ecl_ihs_push(cl_env_copy,&ihs,VV[13],_ecl_debug_env);
TTL:
   {
    volatile long double V1;                      /*  DX              */
    volatile cl_object V2;                        /*  DT              */
    volatile cl_object V3;                        /*  LEN             */
    volatile cl_object V4;                        /*  X               */
    volatile cl_object V5;                        /*  U               */
    V1 = ecl_to_long_double(ecl_divide(VV[7],ecl_make_fixnum(8)));
    V2 = ecl_expt(ecl_make_long_float(V1),ecl_make_fixnum(3));
    T0 = ecl_times(ecl_make_fixnum(20),VV[7]);
    T1 = ecl_divide(T0,ecl_make_long_float(V1));
    V3 = ecl_floor1(T1);
    T0 = (ECL_SYM("FLOAT",374)->symbol.gfdef);
    T1 = ecl_divide(V3,ecl_make_fixnum(-2));
    T2 = ecl_times(ecl_make_long_float(V1),T1);
    T3 = ecl_divide(V3,ecl_make_fixnum(2));
    T4 = ecl_times(ecl_make_long_float(V1),T3);
    T5 = ecl_function_dispatch(cl_env_copy,VV[26])(6, ECL_SYM("START",1310), T2, ECL_SYM("END",1225), T4, VV[10], ecl_make_long_float(V1)) /*  SEQ */;
    V4 = cl_map(3, VV[9], T0, T5);
    T0 = ecl_plus(V3,ecl_make_fixnum(1));
    {
     cl_object V6;                                /*  I               */
     cl_object V7;
     cl_fixnum V8;
     cl_fixnum V9;
     V6 = ECL_NIL;
     {
      if (ecl_unlikely(!ECL_VECTORP(V4))) FEtype_error_vector(V4);
      V7 = V4;
     }
     V8 = 0;
     V9 = 0;
     {
      static const struct ecl_var_debug_info _ecl_descriptors[]={
      {"#:LOOP-ACROSS-LIST12",_ecl_fixnum_loc}
      ,{"#:LOOP-ACROSS-INDEX-11",_ecl_fixnum_loc}
      ,{"#:LOOP-ACROSS-VECTOR-10",_ecl_object_loc}
      ,{"COMMON-LISP-USER::I",_ecl_object_loc}};
      const cl_index _ecl_debug_info_raw[]={
      (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V9),(cl_index)(&V8),(cl_index)(&V7),(cl_index)(&V6)};
      ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,6,,);
      ihs.lex_env = _ecl_debug_env;
      {
       cl_object V10;
       cl_object V11;
       V10 = ecl_list1(ECL_NIL);
       V11 = V10;
       {
        static const struct ecl_var_debug_info _ecl_descriptors[]={
        {"#:LOOP-LIST-TAIL14",_ecl_object_loc}
        ,{"#:LOOP-LIST-HEAD13",_ecl_object_loc}};
        const cl_index _ecl_debug_info_raw[]={
        (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V11),(cl_index)(&V10)};
        ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,4,,);
        ihs.lex_env = _ecl_debug_env;
        V9 = ecl_length(V7);
L15:;
        if (!((V8)>=(V9))) { goto L19; }
        goto L16;
L19:;
        V6 = ecl_aref1(V7,V8);
        {
         cl_object V12;
         V12 = ecl_make_integer((V8)+1);
         {
          static const struct ecl_var_debug_info _ecl_descriptors[]={
          {"#:%CHECKED-VALUE",_ecl_object_loc}};
          const cl_index _ecl_debug_info_raw[]={
          (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V12)};
          ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
          ihs.lex_env = _ecl_debug_env;
          {
          bool V13;
          V13 = ECL_FIXNUMP(V12);
          if (ecl_unlikely(!(V13)))
         FEwrong_type_argument(ECL_SYM("FIXNUM",372),V12);
          }
          V8 = ecl_fixnum(V12);
         }
         ihs.lex_env = _ecl_debug_env;
        }
        T2 = V11;
        T3 = ecl_function_dispatch(cl_env_copy,VV[2])(1, V6) /*  U-INIT */;
        V11 = ecl_list1(T3);
        cl_rplacd(T2, V11);
        goto L15;
L16:;
        T1 = ecl_cdr(V10);
        goto L7;
       }
       ihs.lex_env = _ecl_debug_env;
      }
     }
     ihs.lex_env = _ecl_debug_env;
    }
L7:;
    V5 = cl_make_array(3, T0, ECL_SYM("INITIAL-CONTENTS",1252), T1);
    {
     static const struct ecl_var_debug_info _ecl_descriptors[]={
     {"COMMON-LISP-USER::U",_ecl_object_loc}
     ,{"COMMON-LISP-USER::X",_ecl_object_loc}
     ,{"COMMON-LISP-USER::LEN",_ecl_object_loc}
     ,{"COMMON-LISP-USER::DT",_ecl_object_loc}};
     const cl_index _ecl_debug_info_raw[]={
     (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V5),(cl_index)(&V4),(cl_index)(&V3),(cl_index)(&V2)};
     ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,6,,);
     ihs.lex_env = _ecl_debug_env;
     {
      volatile cl_object V6;                      /*  TEMP            */
      T0 = cl_parse_namestring(1, VV[11]);
      V6 = cl_open(7, T0, ECL_SYM("IF-DOES-NOT-EXIST",1244), ECL_SYM("CREATE",1213), ECL_SYM("IF-EXISTS",1245), ECL_SYM("OVERWRITE",1283), ECL_SYM("DIRECTION",1218), ECL_SYM("OUTPUT",1282));
      {
       static const struct ecl_var_debug_info _ecl_descriptors[]={
       {"COMMON-LISP-USER::TEMP",_ecl_object_loc}};
       const cl_index _ecl_debug_info_raw[]={
       (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V6)};
       ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
       ihs.lex_env = _ecl_debug_env;
       {
        volatile bool unwinding = FALSE;
        cl_index V7=ECL_STACK_INDEX(cl_env_copy),V8;
        ecl_frame_ptr next_fr;
        if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {
          unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;
        } else {
        {
         struct ecl_stack_frame _ecl_inner_frame_aux;
         volatile cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);
         {
          cl_object V9;
          V9 = _ecl_inner_frame;
          {
          static const struct ecl_var_debug_info _ecl_descriptors[]={
          {"#:G18",_ecl_object_loc}};
          const cl_index _ecl_debug_info_raw[]={
          (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V9)};
          ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,3,,);
          ihs.lex_env = _ecl_debug_env;
          {
          cl_object V10;                          /*  TIME            */
          cl_object V11;
          cl_object V12;
          V10 = cl_core.singlefloat_zero;
          V11 = VV[8];
          V12 = V2;
          {
          static const struct ecl_var_debug_info _ecl_descriptors[]={
          {"#:LOOP-STEP-BY20",_ecl_object_loc}
          ,{"#:LOOP-LIMIT19",_ecl_object_loc}
          ,{"COMMON-LISP:TIME",_ecl_object_loc}};
          const cl_index _ecl_debug_info_raw[]={
          (cl_index)(_ecl_debug_env),(cl_index)(_ecl_descriptors),(cl_index)(&V12),(cl_index)(&V11),(cl_index)(&V10)};
          ecl_def_ct_vector(_ecl_debug_env,ecl_aet_index,_ecl_debug_info_raw,5,,);
          ihs.lex_env = _ecl_debug_env;
L40:;
          if (!(ecl_number_compare(V10,V11)>0)) { goto L42; }
          goto L41;
L42:;
          T0 = ecl_function_dispatch(cl_env_copy,VV[27])(1, V5) /*  COERCE-TO-LIST */;
          T1 = ecl_function_dispatch(cl_env_copy,VV[27])(1, V4) /*  COERCE-TO-LIST */;
          T2 = ecl_function_dispatch(cl_env_copy,VV[28])(2, T0, T1) /*  INTERLEAVE */;
          cl_format(4, V6, VV[12], V10, T2);
          V5 = ecl_function_dispatch(cl_env_copy,VV[6])(4, V5, V10, V2, ecl_make_long_float(V1)) /*  UPDATE */;
          V10 = ecl_plus(V10,V2);
          V10 = ecl_plus(V10,V12);
          goto L40;
L41:;
          cl_env_copy->values[0] = ECL_NIL;
          cl_env_copy->nvalues = 1;
          }
          ihs.lex_env = _ecl_debug_env;
          }
          ecl_stack_frame_push_values(V9);
          if (Null(V6)) { goto L52; }
          cl_close(1, V6);
L52:;cl_env_copy->values[0]=ecl_stack_frame_pop_values(V9);
          }
          ihs.lex_env = _ecl_debug_env;
         }
         ecl_stack_frame_close(_ecl_inner_frame);
        }
        }
        ecl_frs_pop(cl_env_copy);
        V8=ecl_stack_push_values(cl_env_copy);
        if (Null(V6)) { goto L54; }
        cl_close(3, V6, ECL_SYM("ABORT",1195), ECL_T);
L54:;
        ecl_stack_pop_values(cl_env_copy,V8);
        if (unwinding) ecl_unwind(cl_env_copy,next_fr);
        ECL_STACK_SET_INDEX(cl_env_copy,V7);
        ecl_ihs_pop(cl_env_copy);
        return cl_env_copy->values[0];
       }
      }
      ihs.lex_env = _ecl_debug_env;
     }
    }
    ihs.lex_env = _ecl_debug_env;
   }
  }
 }
}

#include "kdv.data"
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
 flag->cblock.source = make_constant_base_string("/home/tucker/Repo/my-code/lisp/kdv.lisp");
 return;}
 #ifdef ECL_DYNAMIC_VV
 VV = Cblock->cblock.data;
 #endif
 Cblock->cblock.data_text = "@EcLtAg:init_fas_CODE@";
 VVtemp = Cblock->cblock.temp_data;
 ECL_DEFINE_SETF_FUNCTIONS
 cl_use_package(1, VV[0]);
 ecl_cmp_defmacro(VV[14]);                        /*  ARRAY-LEN       */
 ecl_cmp_defun(VV[17]);                           /*  U-INIT          */
 ecl_cmp_defun(VV[18]);                           /*  U-DISCRETE      */
 ecl_function_dispatch(cl_env_copy,VV[19])(3, VV[4], ECL_SYM("FUNCTION",396), VVtemp[0]) /*  SET-DOCUMENTATION */;
 ecl_cmp_defun(VV[21]);                           /*  U-DISCRETE-I    */
 ecl_cmp_defun(VV[22]);                           /*  UPDATE          */
 ecl_cmp_defun(VV[25]);                           /*  MAIN            */
 ecl_function_dispatch(cl_env_copy,VV[19])(3, VV[13], ECL_SYM("FUNCTION",396), VVtemp[1]) /*  SET-DOCUMENTATION */;
}
