#include "scilisp_check.h"
/*
  Sainity tests to insure basic functions used in testing actually work
*/
//insure the cons function works
START_TEST(test_cons){
  sl_obj control = make_cons(sl_1, sl_2);
  sl_obj test_cons = Fcons(sl_1, sl_2);
  ck_assert_sl_equal(control, test_cons);
}
END_TEST
//insure that make_list works properly
//as it makes later tests much easier to write
START_TEST(test_make_list){
  sl_obj control = make_cons(sl_1,
                        make_cons(sl_2,
                             make_cons (sl_3,
                                   make_cons(sl_4,
                                        make_cons(sl_5, NIL)))));
  sl_obj test_cons = make_list(sl_1, sl_2, sl_3, sl_4, sl_5, NIL);
  ck_assert_sl_equal(control, test_cons);
  
  test_cons = sl_list1(sl_5);
  control = make_cons(sl_5, NIL);
  
  ck_assert_sl_equal(control, test_cons);

  test_cons = sl_list2(sl_5, sl_m5);
  control = make_cons(sl_5, make_cons(sl_m5, NIL));

  ck_assert_sl_equal(control, test_cons);
}
END_TEST
START_TEST(test_cadrs){
  sl_obj test_cons = make_list(sl_1, sl_2, sl_3, sl_4, sl_5, NULL);
  ck_assert_sl_eq(Fcar(test_cons), sl_1);
  ck_assert_sl_eq(Fcdr(Fcar(test_cons)), sl_2);
  ck_assert_sl_equal(Fcddr(test_cons), make_list(sl_3, sl_4, sl_5, NIL));
  
  ck_assert_sl_nil(Fcar(NIL), nil);
  ck_assert_sl_nil(Fcdr(NIL), nil);
}
END_TEST

/*
  Tests of the more complex functions that might actually not work
*/
START_TEST(test_reverse){
  sl_obj test_cons = make_list(sl_1, sl_2, sl_3, sl_4, sl_5, NULL);
  sl_obj snoc_tset = make_list(sl_5, sl_4, sl_3, sl_2, sl_1, NULL);
  //nondestructive reverse
  ck_assert_equal(Freverse(test_cons), snoc_tset);
  ck_assert_equal(Freverse(snoc_tset), test_cons);
  
  test_cons = Fnreverse(test_cons);
  
  ck_assert_sl_equal(test_cons, snoc_tset);

  snoc_tset = Fnreverse(snoc_tset);

  ck_assert_sl_equal(snoc_tset, Freverse(test_cons));
}
END_TEST

/* 
   Append will need to be a function taking N args, but
   for now assume it takes 2 args, so that I can test
   the internals without having to implement varable argument 
   functions.
*/
START_TEST(test_append){  
  sl_obj test_cons = make_list(sl_1, sl_2, sl_3, sl_4, sl_5, NULL);
  sl_obj build_1 = make_list(sl_1, sl_2, sl_3, NULL);
  sl_obj build_2 = make_list(sl_4, sl_5, NULL);
  
  ck_assert_sl_equal(Fappend(build1, build2), test_cons);
  
  build_1 = Fnappend(build1, build2);

  ck_assert_sl_equal(build1, test_cons);
}
END_TEST

START_TEST(test_push_pop){
}
