#undef _FORTIFY_SOURCE
#include <regex.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "checks.h"
#include "gc.h"
#include "main.h"

size_t HEAP_SIZE;
uint64_t* STACK_BOTTOM;
uint64_t* HEAP;
volatile uint64_t* HEAP_END asm("HEAP_END");

snakeval_t set_stack_bottom(uint64_t* stack_bottom) {
  STACK_BOTTOM = stack_bottom;
  return 0;
}

uint64_t* FROM_S;
uint64_t* FROM_E;
uint64_t* TO_S;
uint64_t* TO_E;

int ensure_cache(equal_cache** p_cache, int last, int size, int needed) {
  int minneeded = last + needed;
  if (minneeded >= size) {
    int doubled = size * 2;
    int newsize = (doubled > minneeded) ? doubled : minneeded;
    equal_cache* newcache =
        (equal_cache*)realloc(*p_cache, newsize * sizeof(equal_cache));
    if (newcache != NULL) {
      *p_cache = newcache;
      return newsize;
    } else {
      fprintf(stderr, "Internal error while trying to compute equality\n");
      return 0;
    }
  }
  return size;
}

snakeval_t equal(snakeval_t val1, snakeval_t val2) {
  int size = 100;
  equal_cache* cache = (equal_cache*)calloc(size, sizeof(equal_cache));
  int cur = 0;
  int last = 1;
  snakeval_t ans = SNAKE_TRUE;
  cache[cur].left = val1;
  cache[cur].right = val2;
  while (cur < last) {
    val1 = cache[cur].left;
    val2 = cache[cur].right;
    cur++;
    if (val1 == val2) {
      continue;
    }
    if (val1 == SNAKE_NIL || val2 == SNAKE_NIL) {
      ans = SNAKE_FALSE;
      break;
    }
    int found_cached = -1;
    for (int i = 0; i < cur - 1; i++) {
      if (cache[i].left == val1 && cache[i].right == val2) {
        found_cached = i;
        break;
      }
    }
    if (found_cached > -1) {
      continue;
    }
    if ((val1 & TUP_TAG_MASK) == TUP_TAG && (val2 & TUP_TAG_MASK) == TUP_TAG) {
      uint64_t* tup1 = (uint64_t*)(val1 - TUP_TAG);
      uint64_t* tup2 = (uint64_t*)(val2 - TUP_TAG);
      if (tup1[0] != tup2[0]) {
        ans = SNAKE_FALSE;
        break;
      }
      size = ensure_cache(&cache, last, size, tup1[0]);
      if (size == 0) {
        free(cache);
        return SNAKE_FALSE;
      }
      for (int i = 1; i <= tup1[0] / 2; i++) {
        cache[last].left = tup1[i];
        cache[last].right = tup2[i];
        last++;
      }
      continue;
    }
    ans = SNAKE_FALSE;
    break;
  }
  free(cache);
  return ans;
}

void print_help_unknown(FILE* out, snakeval_t val) {
  fprintf(out, "Unknown value: %#018lx", val);
}

void print_help(FILE* out, snakeval_t val) {
  print_help_impl(print_help, print_help_unknown, out, val);
}

/// @brief Print the stack and register arguments at the current state of
///        the Snake program.
///        This takes a snake language value and returns it unchanged to
///        preserve the expression-based semantics of the language.
/// @note  This must be an `EPrim1` instead of an `EApp` because we cannot
///        inspect the stack in the language itself, and it can only be done by
///        the runtime system. This C function also does not follow the snake
///        calling convention that is normally done by `EApp` expressions, so
///        it requires special handling as a primitive. Additionally, if we had
///        made a tail call into a regular `printStack` function, it would
///        overwrite portions of the stack anyway, which is the very thing we
///        want to look at here.
/// @param val value to return
/// @param rsp current value of RSP
/// @param rbp current value of RBP
/// @param num_args number of arguments the current (snake) function took
/// @return
snakeval_t print_stack(snakeval_t val, uint64_t* rsp, uint64_t* rbp,
                       uint64_t args) {
  // this prints the stack pointer in hex
  printf("RSP: %#018lx\t==>  ", (uint64_t)rsp);
  fflush(stdout);
  print_help(stdout, *rsp);
  fflush(stdout);
  // this prints the base pointer in hex
  printf("\nRBP: %#018lx\t==>  ", (uint64_t)rbp);
  fflush(stdout);
  print_help(stdout, *rbp);
  fflush(stdout);
  // this prints the difference in decimal
  printf("\n(difference: %ld)\n", (uint64_t)(rsp - rbp));
  fflush(stdout);
  // this prints the return value in hex
  printf("Requested return val: %#018lx\t==> ", (uint64_t)val);
  fflush(stdout);
  print_help(stdout, val);
  fflush(stdout);
  printf("\n");
  fflush(stdout);
  // this prints the current function arity in decimal
  printf("Num args: %ld\n", args);

  uint64_t* orig_rsp = rsp;

  if (rsp > rbp) {
    printf("Error: RSP and RBP are not properly oriented\n");
    fflush(stdout);
  } else {
    // i cannot believe this is real code written by ben lerner
    for (uint64_t* cur = rsp; cur < STACK_BOTTOM + 3; cur++) {
      // these print the address and value in hex, and the value itself if
      // relevant
      if (cur == STACK_BOTTOM) {
        printf("BOT %#018lx: %#018lx\t==>  old rbp\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else if (cur == rbp) {
        printf("RBP %#018lx: %#018lx\t==>  old rbp\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else if (cur == orig_rsp) {
        printf("    %#018lx: %#018lx\t==>  old rbp\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else if (cur == rbp + 1) {
        printf("    %#018lx: %#018lx\t==>  saved ret\n", (uint64_t)cur, *cur);
        fflush(stdout);
        rsp = rbp + 2;
        rbp = (uint64_t*)(*rbp);
      } else if (cur == STACK_BOTTOM + 2) {
        printf("    %#018lx: %#018lx\t==>  heap\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else {
        printf("    %#018lx: %#018lx\t==>  ", (uint64_t)cur, *cur);
        fflush(stdout);
        print_help(stdout, *cur);
        fflush(stdout);
        printf("\n");
        fflush(stdout);
      }
    }
  }
  return val;
}

/// @brief Take input from `stdin` and parse it to a snakeval. Accepts the
///        literal strings "true" and "false" for booleans, and integers
///        (in decimal) for numbers.
/// @note Re-prompts the user until a usable value has been entered.
/// @return parsed value
snakeval_t input() {
  char buf[128];
  if (scanf("%127s", buf) != 1) {
    fprintf(stderr, "Failed to read input\n");
    exit(1);
  }
  if (strcmp(buf, "true") == 0) {
    return SNAKE_TRUE;
  } else if (strcmp(buf, "false") == 0) {
    return SNAKE_FALSE;
  } else {
    int64_t in;
    regex_t regex;
    char* pattern = "^-?[0-9]+$";
    regcomp(&regex, pattern, REG_EXTENDED);
    int is_num = regexec(&regex, buf, 0, NULL, 0);
    regfree(&regex);

    if (is_num != 0) {
      fprintf(stderr, "invalid value entered, try again: ");
      return input();
    }

    sscanf(buf, "%lu", &in);

    snakeval_t snake_int = I64_TO_SNAKE(in);
    return snake_int;
  }
}

snakeval_t print(snakeval_t val) {
  print_help(stdout, val);
  printf("\n");
  fflush(stdout);
  return val;
}

/// @brief Exit from the current program context. If we are currently in a
///        check block, we restore RSP and RBP, and then jump to the end of
///        the currently-running check block. Otherwise, we exit the program.
/// @param code exit code
__attribute__((noreturn)) void die(int code) {
  if (in_check) {
    in_check = false;

    __asm__ __volatile__("mov rbp, %[rbp_val]\n\t"
                         "mov rsp, %[rsp_val]\n\t"
                         "jmp %[end]"
                         :
                         : [rbp_val] "r"(check_orig_rbp),
                           [rsp_val] "r"(check_orig_rsp), [end] "r"(check_end)
                         : "memory");

    // assert to the compiler that code here is indeed unreachable,
    // because of the jmp above
    __builtin_unreachable();
  } else {
    exit(code);
  }
}

/// @brief Die the program with the given error code. Prints a message to
///        stderr derived from the code and input value.
/// @param val value that caused this error, used when relevant
/// @param idx index value, if relevant. may be garbage when unneeded
/// @param code type of error encountered
void error(snakeval_t val, snakeval_t idx, uint64_t code) {
  char* message;
  bool print_val = true;
  snakeval_t to_print = val;

  if (in_check) {
    fprintf(stderr, "error encountered while running test at ");
    print_span();
    fprintf(stderr, ": ");
  }

  switch (code) {
    case ERR_ARITH_NOT_NUM:
      message = "arithmetic expected number, recieved";
      break;
    case ERR_COMP_NOT_NUM:
      message = "comparison expected number, recieved";
      break;
    case ERR_LOGIC_NOT_BOOL:
      message = "logic operation expected boolean, recieved";
      break;
    case ERR_IF_NOT_BOOL:
      message = "if expected condition to be boolean, recieved";
      break;
    case ERR_OVERFLOW:
      message = "arithmetic operation overflowed";
      print_val = false;
      break;
    case ERR_IDX_NEGATIVE:
      message = "index too small: tuple access on negative index";
      to_print = idx;
      break;
    case ERR_INDEX_HIGH:
      {
        uint64_t* ptr = ((uint64_t*)(val - TUP_TAG));
        uint64_t tup_len = SNAKE_TO_I64(*ptr);

        fprintf(stderr, "index too large: tuple index out of bounds on tuple ");
        print_help(stderr, val);
        // this prints the tuple length in decimal
        fprintf(stderr, " of length %lu, index: ", tup_len);
        print_help(stderr, idx);
        fprintf(stderr, "\n");

        die(code); // the die here is intentional, this error is special
      }
    case ERR_IDX_NOT_NUM:
      message = "tuple access with non-numeric index";
      to_print = idx;
      break;
    case ERR_NIL_DEREF:
      message = "access component of nil: attempted to dereference nil";
      print_val = false;
      break;
    case ERR_NOT_TUPLE:
      message = "attempted to index non-tuple value, expected tuple";
      break;
    case ERR_OOM:
      message = "out of memory";
      print_val = false;
      break;
    case ERR_CALL_NOT_FN:
      message = "attempted to call a non-function value";
      break;
    case ERR_CALL_ARITY:
      {
        char msg[512];
        uint64_t* ptr = ((uint64_t*)(val - CLOSURE_TAG));
        uint64_t real_arity = (uint64_t)SNAKE_TO_I64(*ptr);

        // quality software, this is definitely not a snakeval in this case
        // because we said so
        uint64_t given_arity = SNAKE_TO_I64(idx);

        // this prints the real and given arities in decimal
        sprintf(msg, "tried to call a %lu-argument function with %lu arguments",
                // subtracting one because of the added closure argument
                (real_arity - 1), (given_arity - 1));
        message = msg;
        print_val = false;
        break;
      }
    case ERR_DESTRUCT_NOT_TUP:
      message = "attempted to destructure non-tuple value";
      break;
    case ERR_DESTRUCT_SIZE:
      {
        uint64_t* ptr = ((uint64_t*)(val - TUP_TAG));
        uint64_t len = (uint64_t)SNAKE_TO_I64(*ptr);

        // this prints the length in decimal
        fprintf(stderr, "attempted to destruct a %lu-tuple with ", len);
        print_help(stderr, idx);
        fprintf(stderr, " bindings\n");

        die(code);
      }
    default:
      message = "unknown error occured, this is a bug in the compiler";
      print_val = false;
  }

  // this prints the error message (and maybe the value) as a string
  if (print_val) {
    fprintf(stderr, "%s: ", message);
    print_help(stderr, to_print);
    fprintf(stderr, "\n");
  } else {
    fprintf(stderr, "%s\n", message);
  }

  die(code);
}

/// @brief Attempts to reserve the desired number of bytes in memory, performing
///        garbage collection if needed.
///
/// @param alloc_ptr The current top of the heap (stored in R15), where the next
///                  allocation should occur, if possible.
/// @param bytes_needed The number of bytes of memory needed for allocation
///                     (including padding).
/// @param cur_frame The base pointer of the topmost stack frame of our code
///                  (i.e., RBP).
/// @param cur_stack_top The stack pointer of the topmost stack frame of our
/// code
///                      (i.e., RSP).
/// @return The new top of the heap (i.e., the new value of R15) after garbage
///         collection. This function does not actually allocate the requested
///         bytes_needed space.
///
/// @details
/// If there is insufficient memory available, the program exits. Otherwise,
/// it updates HEAP and HEAP_END to reflect the new heap location after garbage
/// collection.
uint64_t* try_gc(uint64_t* alloc_ptr, uint64_t bytes_needed,
                 uint64_t* cur_frame, uint64_t* cur_stack_top) {
  uint64_t* new_heap = (uint64_t*)malloc((HEAP_SIZE + 15) * sizeof(uint64_t));
  memset(new_heap, 0x7f7f7f7f, (HEAP_SIZE + 15) * sizeof(uint64_t));
  uint64_t* old_heap = HEAP;
  uint64_t* old_heap_end = (uint64_t*)HEAP_END;

  uint64_t* new_r15 = (uint64_t*)(((uint64_t)new_heap + 15) & ~0xF);
  uint64_t* new_heap_end = new_r15 + HEAP_SIZE;

  FROM_S = (uint64_t*)(((uint64_t)HEAP + 15) & ~0xF);
  FROM_E = (uint64_t*)HEAP_END;
  TO_S = new_r15;
  TO_E = new_heap_end;

  // Abort early, if we can't allocate a new to-space
  if (new_heap == NULL) {
    fprintf(stderr, "Out of memory: could not allocate a new semispace for "
                    "garbage collection\n");
    fflush(stderr);
    if (old_heap != NULL)
      free(old_heap);
    exit(ERR_OOM);
  }

#if DO_GC_LOG
  printf("FROM_S = %#018lx, FROM_E = %#018lx, TO_S = %#018lx, TO_E = %#018lx\n",
         (uint64_t)FROM_S, (uint64_t)FROM_E, (uint64_t)(TO_S), (uint64_t)TO_E);
  print_stack(SNAKE_TRUE, cur_stack_top, cur_frame, 0);
#endif

  new_r15 = gc((uint64_t*)STACK_BOTTOM, cur_frame, cur_stack_top, FROM_S,
               (uint64_t*)HEAP_END, new_r15);

#if DO_GC_LOG
  smarter_print_heap(FROM_S, FROM_E, TO_S, TO_E);
  printf("\n");
#endif

  HEAP = new_heap;
  HEAP_END = new_heap_end;
  free(old_heap);

  // Note: strict greater-than is correct here: if new_r15 + (bytes_needed / 8)
  // == HEAP_END, that does not mean we're *using* the byte at HEAP_END, but
  // rather that it would be the next free byte, which is still ok and not a
  // heap-overflow.

#if DO_GC_LOG
  fprintf(stdout, "words_needed: %ld, heap_size: %ld\n",
          (bytes_needed / sizeof(uint64_t)), HEAP_SIZE);
#endif
  if ((bytes_needed / sizeof(uint64_t)) > HEAP_SIZE) {
    fprintf(
        stderr,
        "Allocation error: needed %ld words, but the heap is only %ld words\n",
        bytes_needed / sizeof(uint64_t), HEAP_SIZE);
    fflush(stderr);
    if (new_heap != NULL)
      free(new_heap);
    exit(ERR_OOM);
  } else if ((new_r15 + (bytes_needed / sizeof(uint64_t))) > HEAP_END) {
#if DO_GC_LOG
    fprintf(stdout,
            "new_r15: %#018lx, bytes_needed: %#018lx, expr: %#018lx, HEAP_END: "
            "%#018lx\n",
            (uint64_t)new_r15, (bytes_needed),
            (uint64_t)(new_r15 + (bytes_needed / sizeof(uint64_t))),
            (uint64_t)HEAP_END);
#endif
    fprintf(stderr,
            "Out of memory: needed %ld words, but only %ld remain after "
            "collection\n",
            bytes_needed / sizeof(uint64_t), (HEAP_END - new_r15));
    fflush(stderr);
    if (new_heap != NULL)
      free(new_heap);
    exit(ERR_OOM);
  } else {
#if DO_GC_LOG
    fprintf(stderr, "new_r15 = %#018lx\n", (uint64_t)new_r15);
    /* naive_print_heap(HEAP, HEAP_END); */
#endif
    if (((uint64_t)new_r15) % 16 != 0) {
      __builtin_trap();
    }

    if (alloc_ptr == new_r15) {
      __builtin_trap();
    }

    return new_r15;
  }
}

int main(int argc, char** argv) {
  HEAP_SIZE = 100000;
  if (argc > 1)
    HEAP_SIZE = atoi(argv[1]);
  if (HEAP_SIZE < 0 || HEAP_SIZE > 1000000)
    HEAP_SIZE = 0;
  HEAP = (uint64_t*)malloc((HEAP_SIZE + 15) * sizeof(uint64_t));
  memset(HEAP, 0x7f7f7f7f, (HEAP_SIZE + 15) * sizeof(uint64_t));

  uint64_t* aligned = (uint64_t*)(((uint64_t)HEAP + 15) & ~0xF);
  HEAP_END = aligned + HEAP_SIZE;

#if DO_GC_LOG
  printf("HEAP = %p, aligned = %p, HEAP_END = %p\n", HEAP, aligned, HEAP_END);
#endif

  snakeval_t result = our_code_starts_here(aligned, HEAP_SIZE);

#if DO_GC_LOG
  smarter_print_heap(aligned, (uint64_t*)HEAP_END, TO_S, TO_E);
#endif
  print(result);

  free(HEAP);
  return 0;
}
