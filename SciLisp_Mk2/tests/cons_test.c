#include "scilisp_check.h"
//insure the cons function works
START_TEST(test_cons){
  sl_obj control = make_cons(make_int(1), make_int(2));
  sl_obj test_cons = Fcons(make_int(1), make_int(2));
  ck_assert_sl_equal(control, test_cons);
}
END_TEST
START_TEST(test_cadrs){
  sl_obj test_cons = make_int_list(1,2,3,4,5,NULL);
  ck_assert_sl_eq(Fcar(test_cons), make_int(1));
}
END_TEST

START_TEST(test_cdr){
  sl_obj test_cons = Fcons(make_int(1), make_int(2));
  ck_assert_sl_eq(Fcdr(test_cons), make_int(2));
}
END_TEST

