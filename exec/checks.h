#include "snake.h"
#include <regex.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define EQUAL_TAG 0b00
#define EQ_TAG 0b01
#define PRED_TAG 0b10
#define SAT_TAG 0b11
#define NEG_FLAG 0b0100
#define BECAUSE_FLAG 0b1000

extern snakeval_t start_check_block(void* orig_rbp, void* orig_rsp,
                                    void* end) asm("start_check_block");
extern snakeval_t init_test(snakeval_t snakeval) asm("init_test");
extern snakeval_t test(snakeval_t tag, snakeval_t t1,
                       snakeval_t t2) asm("test");
extern snakeval_t end_check_block() asm("end_check_block");

typedef struct sourcespan {
  uint64_t start_l;
  uint64_t start_c;
  uint64_t end_l;
  uint64_t end_c;
} sourcespan_t;

// `curr_span` and `in_check` together act as an optional.
// if `in_check` is false, the span is garbage.
extern sourcespan_t curr_span;
extern bool in_check;
extern void* check_orig_rbp;
extern void* check_orig_rsp;
extern void* check_end;

#define INTERNAL_ERROR -1

void print_span(); // not extern/asm
