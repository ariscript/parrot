#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define TRANSMUTE(ty, val) ((*((ty*)&(val))))

#define SNAKE_TRUE 0xffffffffffffffff
#define SNAKE_FALSE 0x7fffffffffffffff
#define SNAKE_NIL 0x0000000000000001 // ewww

#define BOOL_TAG 0x0000000000000007 // 0b111
#define BOOL_TAG_MASK 0x0000000000000007
#define NUM_TAG 0x0000000000000000
#define NUM_TAG_MASK 0x0000000000000001
#define TUP_TAG 0x0000000000000001 // 0b001
#define TUP_TAG_MASK 0x0000000000000007
#define CLOSURE_TAG 0x0000000000000005 // 0b101
#define CLOSURE_TAG_MASK 0x0000000000000007
#define FORWARD_TAG 0x0000000000000003 // 0b011
#define FORWARD_TAG_MASK 0x0000000000000007

#define ERR_COMP_NOT_NUM 1
#define ERR_ARITH_NOT_NUM 2
#define ERR_LOGIC_NOT_BOOL 3
#define ERR_IF_NOT_BOOL 4
#define ERR_OVERFLOW 5
#define ERR_NOT_TUPLE 6
#define ERR_IDX_NEGATIVE 7
#define ERR_INDEX_HIGH 8
#define ERR_IDX_NOT_NUM 9
#define ERR_NIL_DEREF 10
#define ERR_OOM 11
#define ERR_CALL_NOT_FN 16
#define ERR_CALL_ARITY 17
#define ERR_DESTRUCT_NOT_TUP 18
#define ERR_DESTRUCT_SIZE 19

#define STACK_PAD 0xbeefbeefbeefbeef
#define STACK_INIT 0xdeadbeefdeadbeef

typedef uint64_t snakeval_t;

#define SNAKE_TO_I64(s) (TRANSMUTE(int64_t, (s)) >> 1)
#define I64_TO_SNAKE(s) (TRANSMUTE(uint64_t, (s)) << 1)

typedef void (*printer_t)(FILE*, snakeval_t);

void print_help_impl(printer_t this, printer_t unknown, FILE* out,
                     snakeval_t val);
extern snakeval_t equal(snakeval_t left, snakeval_t right) asm("equal");
#define DO_GC_LOG 0
#define DO_TEST_LOG 0
