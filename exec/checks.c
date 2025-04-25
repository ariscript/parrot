#undef _FORTIFY_SOURCE
#include "checks.h"
#include "main.h"

sourcespan_t curr_span;
bool in_check = false;
void* check_orig_rbp = NULL;
void* check_orig_rsp = NULL;
void* check_end = NULL;

snakeval_t start_check_block(void* orig_rbp, void* orig_rsp, void* end) {
#if DO_TEST_LOG
  printf("running start_check_block\n");
#endif

  if (in_check == true) {
    fprintf(stderr, "wtf we already checking\n");
    exit(INTERNAL_ERROR);
  }

  check_orig_rbp = orig_rbp;
  check_orig_rsp = orig_rsp;
  check_end = end;

  in_check = true;

  return SNAKE_NIL;
}

uint64_t ran_tests = 0;
uint64_t passed_tests = 0;

snakeval_t init_test(snakeval_t snakeval) {
#if DO_TEST_LOG
  printf("running init_test\n");
#endif

  if ((snakeval & TUP_TAG_MASK) != TUP_TAG) {
    fprintf(stderr, "expected span to be tuple\n");
    exit(INTERNAL_ERROR);
  }

  uint64_t* raw_ptr = (uint64_t*)(snakeval - TUP_TAG);

  if (raw_ptr[0] != (4 * 2)) {
    fprintf(stderr, "tuple to be length 4\n");
    exit(INTERNAL_ERROR);
  }

  curr_span.start_l = SNAKE_TO_I64(raw_ptr[1]);
  curr_span.start_c = SNAKE_TO_I64(raw_ptr[2]);
  curr_span.end_l = SNAKE_TO_I64(raw_ptr[3]);
  curr_span.end_c = SNAKE_TO_I64(raw_ptr[4]);

  return SNAKE_NIL;
}

snakeval_t test(snakeval_t tag, snakeval_t t1, snakeval_t t2) {
#if DO_TEST_LOG
  printf("running test\n");
#endif

  uint64_t raw_tag = SNAKE_TO_I64(tag);

  bool is_neg = (raw_tag & NEG_FLAG) == NEG_FLAG;
  bool is_because = (raw_tag & BECAUSE_FLAG) == BECAUSE_FLAG;

  uint64_t base_tag = (raw_tag & 0b11);

  bool is_trueish = false;
  bool print_right = true;
  bool success = false;
  char* msg;
  char* because_msg =
      is_because ? "explanation and right side" : "left and right sides";

  switch (base_tag) {
    case EQUAL_TAG:
      {
        is_trueish = (equal(t1, t2) == SNAKE_TRUE);
        msg = !is_neg ? "were not `equal`" : "were `equal`";
        break;
      }
    case EQ_TAG:
      {
        is_trueish = t1 == t2;
        msg = !is_neg ? "were not `==`" : "were `==`";
        break;
      }
    case PRED_TAG:
    case SAT_TAG:
      {
        // here, t2 is mostly meaningless, but we pass in the predicate in
        // case it can ever be useful later
        char* where = (base_tag == PRED_TAG)
                          ? because_msg
                          : (is_because ? "explanation" : "left side");

        if ((t1 & BOOL_TAG_MASK) != BOOL_TAG) {
          fprintf(stderr, "while running test at ");
          print_span();
          fprintf(stderr, ", the predicate returned a non-boolean value (");
          print_help(stderr, t1);
          fprintf(stderr, ") when called on the %s\n", where);

          if (!is_because) {
            ran_tests += 1;
          }

          return SNAKE_NIL;
        }

        is_trueish = t1 == SNAKE_TRUE;

        because_msg = where;
        print_right = false;
        msg = !is_neg ? "failed the predicate" : "passed the predicate";
        break;
      }
  }

  success = is_trueish == !is_neg;

  if (!success) {
    fprintf(stderr, "failed test at ");
    print_span();
    fprintf(stderr, ": the %s %s (", because_msg, msg);
    print_help(stderr, t1);
    if (print_right) {
      fprintf(stderr, " and ");
      print_help(stderr, t2);
    }
    fprintf(stderr, ")\n");
  } else if (!is_because) {
    passed_tests += 1;
  }

  if (!is_because) {
    ran_tests += 1;
  }

#if DO_TEST_LOG
  fprintf(stderr, "end test\n");
#endif

  return SNAKE_NIL;
}

snakeval_t end_check_block() {
#if DO_TEST_LOG
  printf("running end_check_block\n");
#endif

  in_check = false;
  fprintf(stderr, "check block: %ld/%ld tests passed\n", passed_tests,
          ran_tests);
  ran_tests = 0;
  passed_tests = 0;
  return SNAKE_NIL;
}

void print_span() {
  fprintf(stderr, "%ld:%ld-%ld:%ld", curr_span.start_l, curr_span.start_c,
          curr_span.end_l, curr_span.end_c);
}
