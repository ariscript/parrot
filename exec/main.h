#include <stdint.h>

#include "snake.h"

extern snakeval_t
set_stack_bottom(uint64_t* stack_bottom) asm("set_stack_bottom");
extern snakeval_t our_code_starts_here(uint64_t* heap,
                                       int size) asm("our_code_starts_here");
extern snakeval_t print(snakeval_t val) asm("print");
extern snakeval_t print_stack(snakeval_t val, uint64_t* rsp, uint64_t* rbp,
                              uint64_t num_args) asm("print_stack");
extern snakeval_t input() asm("input");
extern void error(snakeval_t val, snakeval_t idx, uint64_t code) asm("error");
extern uint64_t* try_gc(uint64_t* alloc_ptr, uint64_t bytes_needed,
                        uint64_t* cur_frame,
                        uint64_t* cur_stack_top) asm("try_gc");

typedef struct equal_cache {
  snakeval_t left;
  snakeval_t right;
} equal_cache;

void print_help(FILE* out, snakeval_t val); // not extern/asm
