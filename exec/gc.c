#undef _FORTIFY_SOURCE
#include "gc.h"
#include "snake.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h> // c moment

#define WORD_SIZE 8
#define INCR_PTR(ptr, amnt) ((uint64_t*)(((uint64_t)(ptr)) + (amnt)))

#define IS_TAGGED(val, mask, tag) ((((val) & (mask)) == (tag)))
#define SEGFAULT_PLZ __builtin_trap()

extern uint64_t* STACK_BOTTOM;
extern uint64_t* FROM_S;
extern uint64_t* FROM_E;
extern uint64_t* TO_S;
extern uint64_t* TO_E;

/// @brief Prints the contents of the heap in a raw format.
///
/// @param heap The starting address of the heap.
/// @param heap_end The first address after the heap.
///
/// @details
/// This function prints each word in the heap, displaying the word number,
/// exact address, hex value at that address, and decimal value at that address.
/// It does not attempt to interpret the words as Garter values.
void naive_print_heap(uint64_t* heap, uint64_t* heap_end) {
  printf("In naive_print_heap from %p to %p\n", heap, heap_end);
  for (uint64_t i = 0; i < (uint64_t)(heap_end - heap); i += 1) {
    printf("  %ld/%p: %p (%ld)\n", i, (heap + i), (uint64_t*)(*(heap + i)),
           *(heap + i));
  }
}

void print_help_val_unknown(FILE* out, snakeval_t val) {
  fprintf(out, "%#018lx", val);
}

void print_heap_val(FILE* out, uint64_t val) {
  print_help_impl(print_heap_val, print_help_val_unknown, out, val);
}

/// @brief Prints the contents of the heap using a smarter algorithm.
///
/// @param from_start The starting address (inclusive) of the from-space of the
///                   heap.
/// @param from_end The ending address (exclusive) of the from-space of the
///                 heap.
/// @param to_start The starting address (inclusive) of the to-space of the
///                 heap.
/// @param to_end The ending address (exclusive) of the to-space of the heap.
void smarter_print_heap(uint64_t* from_start, uint64_t* from_end,
                        uint64_t* to_start, uint64_t* to_end) {
  printf("In smarter_print_heap from %p to %p\n", from_start, from_end);
  for (uint64_t i = 0; i < (uint64_t)(from_end - from_start); i += 1) {
    printf("  %ld/%#018lx: %#018lx (", i, (uint64_t)(from_start + i),
           (uint64_t)(*(from_start + i)));
    print_heap_val(stdout, *(from_start + i));
    printf(")\n");
  }
  printf("In smarter_print_heap from %p to %p\n", to_start, to_end);
  for (uint64_t i = 0; i < (uint64_t)(to_end - to_start); i += 1) {
    printf("  %ld/%#018lx: %#018lx (", i, (uint64_t)(to_start + i),
           (uint64_t)(*(to_start + i)));
    print_heap_val(stdout, *(to_start + i));
    printf(")\n");
  }
}

/// @return the number of WORDS
uint64_t size_of_heap_val(uint64_t* heap_thing_addr, uint64_t tag) {
  if (((uint64_t)heap_thing_addr) % 16 != 0) {
    SEGFAULT_PLZ;
  }

  uint64_t needed;
  snakeval_t snake_size;
  uint64_t* untagged_ptr;
  if (tag == TUP_TAG) {
    /* [size, <*size* items...>] */
    snake_size = heap_thing_addr[0];
    needed = SNAKE_TO_I64(snake_size) + 1;
  } else if (tag == CLOSURE_TAG) {
    /* [arity, code_ptr, num_free, <*num_free* frees...>] */
    snake_size = heap_thing_addr[2];
    needed = SNAKE_TO_I64(snake_size) + 3;
  } else {
    fprintf(stderr, "what in ohio v2\n");
    exit(-69);
  }

  uint64_t padding = needed % 2 == 0 ? 0 : 1;
  uint64_t size = needed + padding;
  if (size > 100000) {
    SEGFAULT_PLZ;
  }
  return size;
}

/// @brief Copies a Garter value from the given address to the new heap,
///        but only if the value is heap-allocated and needs copying.
///
/// @param garter_val_addr The *address* of some Garter value, which contains a
///                        Garter value, i.e., a tagged word. It may or may not
///                        be a pointer to a heap-allocated value.
/// @param heap_top The location at which to begin copying, if any copying is
///                 needed.
/// @return The new top of the heap, at which to continue allocations.
///
/// @details
/// If the data needed to be copied, this function replaces the value at its
/// old location with a forwarding pointer to its new location.
uint64_t* copy_if_needed(snakeval_t* garter_val_addr, uint64_t* heap_top) {
  if (((uint64_t)heap_top) % 16 != 0) {
    SEGFAULT_PLZ;
  }

  uint64_t* orig_heap_top = heap_top;

  snakeval_t garter_val = *garter_val_addr;

  // If garter_val is a primitive (number or boolean), return the unchanged
  // heap_top; nothing needs to be allocated.
  if (garter_val == SNAKE_TRUE || garter_val == SNAKE_FALSE ||
      garter_val == SNAKE_NIL || IS_TAGGED(garter_val, NUM_TAG_MASK, NUM_TAG)) {
    return heap_top;
  }

  if (garter_val == STACK_PAD || garter_val == STACK_INIT) {
    return heap_top;
  }

  if (garter_val == 0x7f7f7f7f7f7f7f7fL) {
#if DO_GC_LOG
    printf("FOUND UNINIT HEAP VALUE at %p\n", garter_val_addr);
#endif
    return heap_top;
  }

  // If garter_val is a (tagged) pointer to a heap-allocated Garter value (tuple
  // or closure): Call the pointed-to value heap_thing, such that
  // untag(garter_val) = heap_thing_addr, then
  uint64_t tag;
  if (IS_TAGGED(garter_val, TUP_TAG_MASK, TUP_TAG)) {
    tag = TUP_TAG;
  } else if (IS_TAGGED(garter_val, CLOSURE_TAG_MASK, CLOSURE_TAG)) {
    tag = CLOSURE_TAG;
  } else {
    fprintf(stderr, "what in ohio: %#018lx\n", garter_val);
    exit(-69);
  }

  uint64_t* heap_thing_addr = (uint64_t*)(garter_val - tag);
  snakeval_t heap_thing = *heap_thing_addr;

  // If garter_val is a (tagged) pointer to a heap_thing that now contains a
  // forwarding pointer, replace the value at garter_val_addr with the
  // appropriately tagged version of that forwarding pointer
  if (IS_TAGGED(heap_thing, FORWARD_TAG_MASK, FORWARD_TAG)) {
    uint64_t* untaged_forwarded_snake_ptr =
        ((uint64_t*)(heap_thing - FORWARD_TAG));

    if (((uint64_t)heap_top) % 16 != 0) {
      SEGFAULT_PLZ;
    }

#if DO_GC_LOG
    printf("FOUND FWD PTR TO %#018lx\n",
           (snakeval_t)untaged_forwarded_snake_ptr);
#endif
    *garter_val_addr = (snakeval_t)untaged_forwarded_snake_ptr + tag;
    return heap_top;
  }

  uint64_t words = size_of_heap_val(heap_thing_addr, tag);
  uint64_t bytes = words * WORD_SIZE;

  memcpy(heap_top, heap_thing_addr, bytes);
#if DO_GC_LOG
  printf("%#018lx is of size %ld words\n", (uint64_t)garter_val_addr, words);
#endif

  *garter_val_addr = (snakeval_t)INCR_PTR(heap_top, tag);
  // Replace the value at heap_thing_addr (i.e., the location referred to by
  // garter_val) with a forwarding pointer to heap_top. A forwarding pointer
  // needs to be tagged (so that you can reliably detect that it is not some
  // other Garter value)
  *heap_thing_addr = (uint64_t)INCR_PTR(heap_top, FORWARD_TAG);
  // heap_top = INCR_PTR(heap_top, bytes);
  heap_top += words;

  if (tag == TUP_TAG) {
    // one because of size field
    for (uint64_t i = WORD_SIZE; i < bytes; i += WORD_SIZE) {
      snakeval_t* garter_field_addr = INCR_PTR(orig_heap_top, i);
      if (((uint64_t)garter_field_addr) % 8 != 0) {
        SEGFAULT_PLZ;
      }
      heap_top = copy_if_needed(garter_field_addr, heap_top);
    }
  } else if (tag == CLOSURE_TAG) {
    // skip over arity, ptr, free
    for (uint64_t i = 3 * WORD_SIZE; i < bytes; i += WORD_SIZE) {
      snakeval_t* garter_field_addr = INCR_PTR(orig_heap_top, i);
      if (((uint64_t)garter_field_addr) % 8 != 0) {
        SEGFAULT_PLZ;
      }
      heap_top = copy_if_needed(garter_field_addr, heap_top);
    }
  }

  if (((uint64_t)heap_top) % 16 != 0) {
    SEGFAULT_PLZ;
  }
  return heap_top;
}

/// @brief Implements Cheney's garbage collection algorithm.
///
/// @param bottom_frame The base pointer of our_code_starts_here, i.e., the
///                     bottommost Garter frame.
/// @param top_frame The base pointer of the topmost Garter stack frame.
/// @param top_stack The current stack pointer of the topmost Garter stack
///                  frame.
/// @param from_start The beginning of the from-space of memory that is being
///                   compacted.
/// @param from_end The end of the from-space of memory that is being compacted.
/// @param to_start The beginning of the to-space of memory.
/// @return The new location within to_start at which to allocate new data.
uint64_t* gc(uint64_t* bottom_frame, uint64_t* top_frame, uint64_t* top_stack,
             uint64_t* from_start, uint64_t* from_end, uint64_t* to_start) {
  uint64_t* old_top_frame = top_frame;
  do {
    // CAREFULLY CONSIDER: do you need `top_stack + 1`?
    for (uint64_t* cur_word = top_stack; cur_word < top_frame; cur_word++) {
#if DO_GC_LOG
      // naive_print_heap(from_start, from_end, 0, 0);
      // printf("\n");
#endif
      to_start = copy_if_needed(cur_word, to_start);
#if DO_GC_LOG
      // smarter_print_heap(0, 0, TO_S, TO_E);
      // printf("\n");
#endif
    }
    /* Shift to next stack frame:
     * [top_frame] points to the saved RBP, which is the RBP of the next stack
     * frame
     * [top_frame + 8] is the return address, and
     * [top_frame + 16] is therefore the next frame's stack-top
     */
    top_stack = top_frame + 2;
    old_top_frame = top_frame;
    top_frame = (uint64_t*)(*top_frame);
  } while (old_top_frame < bottom_frame); // Use the old stack frame to decide
                                          // if there's more GC'ing to do
  // CAREFULLY CONSIDER: Should this be `<=` or `<`?

  // after copying and GC'ing all the stack frames, return the new allocation
  // starting point
  return to_start;
}
