/* main file for running tests, doesn't contain any tests itself*/
#include "scilisp_check.h"
START_TEST(test_fail){
  ck_abort_msg("Expected Test Failure");
}
END_TEST

START_TEST(test_pass){
  ck_assert(1);
}
END_TEST

/* gen_test_case_fn(sanity, 1, test_pass); */
/* gen_test_case_fn(fail, 1, test_fail); */
/* gen_test_suite_fn(core, 2, test_sanity, test_fail); */

int main(int argc, char *argv[]){
  TCase *tc_sanity = gen_test_case("sanity", 1, test_pass);
  TCase *tc_fail = gen_test_case("fail", 1, test_fail);
  Suite *s = gen_test_suite("core", 2, tc_sanity, tc_fail);
  SRunner *sr;
  int num_failed;
  sr = srunner_create(s);
  //sainity test
  srunner_run(sr, NULL, "sanity", CK_NORMAL);
  num_failed = srunner_ntests_failed(sr);
  if (num_failed > 0) {
    fprintf(stderr,"Sanity test failed\n");
    return 1;
  }
  //Make sure tests can fail properly
  printf("Next test(s) expected to fail\n");
  srunner_run(sr, NULL, "fail", CK_NORMAL);
  num_failed = srunner_ntests_failed(sr);
  srunner_free(sr);
  return (num_failed == 1);
}
