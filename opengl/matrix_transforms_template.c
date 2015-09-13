#if !(defined TYPE) || !(defined TYPE_LETTER)
#error "TYPE and TYPE_LETTER must be defined before including"  \
  "matrix_transforms_template.c\n"
/*
  Visual for matrix indices:
  Row Major     Column Major
  |0 |1 |2 |3 | |0 |4 |8 |12|  
  |4 |5 |6 |7 | |1 |5 |9 |13|
  |8 |9 |10|11| |2 |6 |10|14|
  |12|13|14|15| |3 |7 |11|15|
*/
/*
  Generalized rotation matrix R around unit vector (l,m,n) by θ(taken from wikipedia):
  |ll(1-cos(θ)) +  (cos(θ))|ml(1-cos(θ)) - n(sin(θ))|nl(1-cos(θ)) + m(sin(θ))|
  |lm(1-cos(θ)) + n(sin(θ))|mm(1-cos(θ)) -  (cos(θ))|nm(1-cos(θ)) - l(sin(θ))|
  |ln(1-cos(θ)) + m(sin(θ))|mn(1-cos(θ)) - l(sin(θ))|nn(1-cos(θ)) +  (cos(θ))|
  or R = cos(θ)I + sin(θ)[u]ₓ + (1-cos(θ))u * u
  where * is the tensor product, and [u]ₓ is the cross product matrix of u
*/
#define sincosd sincos
#define sin_cos(t, s, c) sincos##TYPE_LETTER(t,s,c)
#define cblas_mat cblas_##TYPE_LETTER##mat
#define cblas_vec cblas_##TYPE_LETTER##vec
TYPE *generalized_rotation_f(TYPE theta, TYPE l, TYPE m, TYPE n){
  TYPE s,c;
  sincos(theta, &s, &c);
  TYPE unit_vec[4] = {l,m,n,1};
  cblas_vec unit = {.data = unit_vec,.inc = 1,.n = 4};
  //cross product matrix
  TYPE scratch_data[16] = {[1] = -n*s, [2] = m*s, [4] = n*s,
                            [6] = -l*s, [8] = -m*s, [9] = l*s, [15] = 1};
  cblas_mat scratch = make_mat_square(scratch_data, 4);
  //cos * identity matrix
  TYPE *rot_data = ZMALLOC(4*4*sizeof(TYPE));
  rot_data[0] = rot_dat[5] = rot_data[10] = c;
  rot[15] = 1.0f;//this should be 1 right?
  cblas_mat rot = make_mat_square(rot_data, 4);

  rot = mat_addmul_square(1,scratch,1,rot);//cos(θ)I + sin(θ)[u]ₓ
  rot = sger(1-c, unit, unit, rot);

  return rot.data;//could eaisly return the cblas_mat instead
}

TYPE *generalized_rotation_f(TYPE theta, TYPE l, TYPE m, TYPE n){
  TYPE s,c;
  sincosf(theta, &s, &c);
  TYPE unit_vec[4] = {l,m,n,1};
  cblas_vec unit = {.data = unit_vec,.inc = 1,.n = 4};
  //cross product matrix
  TYPE scratch_data[16] = {[1] = -n*s, [2] = m*s, [4] = n*s,
                            [6] = -l*s, [8] = -m*s, [9] = l*s, [15] = 1};
  cblas_mat scratch = make_mat_square(scratch_data, 4);
  //cos * identity matrix
  TYPE *rot_data = ZMALLOC(4*4*sizeof(TYPE));
  rot_data[0] = rot_dat[5] = rot_data[10] = c;
  rot[15] = 1.0f;//this should be 1 right? or should it be c?
  cblas_mat rot = make_mat_square(rot_data, 4);

  rot = mat_addmul_square(1,scratch,1,rot);//cos(θ)I + sin(θ)[u]ₓ
  rot = sger(1-c, unit, unit, rot);

  return rot.data;//could eaisly return the cblas_mat instead
}

#define sincosd sincos
#define make_rot_fun(type, axis, suffix, a, b, c, d)            \
  SINLINE type *make_rot_##plane##_##suffix(type theta){        \
    type s,c;                                                   \
    sincos##suffix(theta, &s, &c);                              \
    type *rot = ZMALLOC(4*4*sizeof(type));                      \
    rot[a] = rot[b] = c;                                        \
    rot[c] = s; rot[d] = -s;                                    \
    return rot;                                                 \
  }
#define make_rot_funs(axis, a, b, c, d)         \
  make_rot_fun(TYPE, axis, f, a, b, c, d);     \
  make_rot_fun(TYPE, axis, d, a, b, c, d);
//clockwise rotation matrices
make_rot_funs(x, 5, 10, 6, 9);
make_rot_funs(y, 0, 10, 8, 2);
make_rot_funs(z, 0, 5,  1, 4);

static TYPE *translation_matrix_internal_f(TYPE *trans,
                                            TYPE x, TYPE y, TYPE z){
  set_diagonal(trans, 1.0);
  trans[3] = x;
  trans[7] = y;
  trans[11] = z;
  return trans;
}
#define translation_matrix_internal translation_matrix_internal_##TYPE_LETTER
static TYPE *translation_matrix_internal(TYPE *trans,
                                         TYPE x, TYPE y, TYPE z){
  set_diagonal(trans, 1.0);
  trans[3] = x;
  trans[7] = y;
  trans[11] = z;
  return trans;
}
#define make_translation make_translation_##TYPE_LETTER
TYPE *make_translation(TYPE x, TYPE y, TYPE z){
  TYPE *trans = ZMALLOC(4*4*sizeof(TYPE));
  return translation_matrix_internal(trans, x, y, z);
}

#undef translation_matrix_internal
#undef make_translation
#undef sincosd
#undef sin_cos
#undef cblas_mat
#undef cblas_vec
