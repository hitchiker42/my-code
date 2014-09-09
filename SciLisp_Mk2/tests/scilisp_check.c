/* main file for running tests, doesn't contain any tests itself*/
#include "scilisp_check.h"

START_TEST(test_pass){
  ck_assert(1);
}
END_TEST

/* gen_test_case_fn(sanity, 1, test_pass); */
/* gen_test_case_fn(fail, 1, test_fail); */
/* gen_test_suite_fn(core, 2, test_sanity, test_fail); */

int main(int argc, char *argv[]){
  TCase *tc_sanity = gen_test_case("sanity", 1, test_pass);
  Suite *s = gen_test_suite("core", 1, tc_sanity);
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
  srunner_free(sr);
  return 0;
}
