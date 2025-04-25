#undef _FORTIFY_SOURCE
#include "snake.h"
#include <regex.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// @brief An incredible layer of indirection
/// @param
/// @param unknown
/// @param out
/// @param val
void print_help_impl(printer_t this, printer_t unknown, FILE* out,
                     snakeval_t val) {
  static int tuple_counter = 0;

  if (val == SNAKE_NIL) {
    fprintf(out, "nil");
  } else if ((val & NUM_TAG_MASK) == NUM_TAG) {
    fprintf(out, "%ld", SNAKE_TO_I64(val));
  } else if (val == SNAKE_TRUE) {
    fprintf(out, "true");
  } else if (val == SNAKE_FALSE) {
    fprintf(out, "false");
  } else if ((val & CLOSURE_TAG_MASK) == CLOSURE_TAG) {
    uint64_t* addr = (uint64_t*)(val - CLOSURE_TAG);
    fprintf(out,
            "[%p - 5] ==> <function arity %ld, closed %ld, fn-ptr %#018lx>",
            (uint64_t*)val, SNAKE_TO_I64(addr[0]) - 1, SNAKE_TO_I64(addr[2]),
            addr[1]);
  } else if ((val & TUP_TAG_MASK) == TUP_TAG) {
    uint64_t* addr = (uint64_t*)(val - TUP_TAG);
    if ((*addr & 0x8000000000000000) != 0) {
      // this prints the cache id in decimal
      fprintf(out, "<cyclic tuple %d>", (int)(*addr & 0x7FFFFFFFFFFFFFFF));
      return;
    }

    int len = SNAKE_TO_I64(addr[0]);
    *(addr) = 0x8000000000000000 | (++tuple_counter);
    fprintf(out, "(");
    for (int i = 1; i <= len; i++) {
      if (i > 1)
        fprintf(out, ", ");
      this(out, addr[i]);
    }
    if (len == 1)
      fprintf(out, ",");
    fprintf(out, ")");
    // Unmark this tuple: restore its length
    *(addr) = I64_TO_SNAKE(len); // length is encoded
  } else if ((val & FORWARD_TAG_MASK) == FORWARD_TAG) {
    fprintf(out, "<forward pointer to %#018lx>", (val - FORWARD_TAG));
  } else {
    unknown(out, val);
  }
}
