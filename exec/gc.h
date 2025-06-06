#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/// @brief Prints the contents of the heap in a raw format.
///
/// @param heap The starting address of the heap.
/// @param heap_end The first address after the heap.
///
/// @details
/// This function prints each word in the heap, displaying the word number,
/// exact address, hex value at that address, and decimal value at that address.
/// It does not attempt to interpret the words as Garter values.
void naive_print_heap(uint64_t* heap,
                      uint64_t* heap_end) asm("naive_print_heap");

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
                        uint64_t* to_start, uint64_t* to_end);

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
uint64_t* copy_if_needed(uint64_t* garter_val_addr, uint64_t* heap_top);

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
             uint64_t* from_start, uint64_t* from_end, uint64_t* to_start);
