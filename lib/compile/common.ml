open Exprs
open Assembly

let print_env env how =
  debug_printf "Env is\n";
  List.iter (fun (id, bind) -> debug_printf "  %s -> %s\n" id (how bind)) env
;;

module SnakeTags = struct
  let book_val_mask = HexConst 0x8000000000000000L

  let bool_tag = 0x0000000000000007L

  let bool_tag_mask = 0x0000000000000007L

  let num_tag = 0x0000000000000000L

  let num_tag_mask = 0x0000000000000001L

  let tup_tag = 0x0000000000000001L

  let tup_tag_mask = 0x0000000000000007L

  let closure_tag = 0x0000000000000005L

  let closure_tag_mask = 0x0000000000000007L
end

module SnakeVal = struct
  let s_true = HexConst 0xFFFFFFFFFFFFFFFFL

  let s_false = HexConst 0x7FFFFFFFFFFFFFFFL

  let one = Const 2L

  let nil = HexConst SnakeTags.tup_tag
end

module SnakeErrors = struct
  let comp_not_num = 1L

  let arith_not_num = 2L

  let logic_not_bool = 3L

  let if_not_bool = 4L

  let overflow = 5L

  let not_tuple = 6L

  let idx_negative = 7L

  let index_high = 8L

  let idx_not_num = 9L

  let nil_deref = 10L

  let oom = 11L

  let call_not_fn = 16L

  let call_arity = 17L

  let destruct_not_tup = 18L

  let destruct_size = 19L
end

module Regs = struct
  let first_six_args = [|RDI; RSI; RDX; RCX; R8; R9|]

  let caller_saved = [Reg RDI; Reg RSI; Reg RDX; Reg RCX; Reg R8; Reg R9]

  let callee_saved = [Reg R12; Reg R13; Reg R14]

  let color = caller_saved @ callee_saved

  let heap = R15

  let scratch = R11

  let scratch2 = R10
end

let builtins =
  [("print", 1); ("equal", 2); ("input", 0)]
  |> List.mapi (fun i (name, ar) -> (name, (Native, ar, None, i * 4)))
;;

let stack_padding = 0xbeefbeefbeefbeefL

let snake_uninit = 0xdeadbeefdeadbeefL

let dummy_span = (Lexing.dummy_pos, Lexing.dummy_pos)

let snake_of_int x = x |> Int.mul 2 |> Int64.of_int
