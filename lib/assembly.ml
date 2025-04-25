open Printf

(* Abstract syntax of (a small subset of) x86 assembly instructions *)
let word_size = 8

type reg =
  | RAX
  | RSP
  | RBP
  | RSI
  | RDI
  | RDX
  | RCX
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | CL
[@@deriving show {with_path= false}]

type size =
  | QWORD_PTR
  | DWORD_PTR
  | WORD_PTR
  | BYTE_PTR
[@@deriving show {with_path= false}]

type arg =
  | Const of int64
  | HexConst of int64
  | Reg of reg
  | RegOffset of int * reg (* int is # words of offset *)
  | RegOffsetReg of reg * reg * int * int
  | Sized of size * arg
  | LabelContents of string
  | Label of string
  | RelLabel of string
[@@deriving variants, show {with_path= false}]

type instruction =
  | IMov of arg * arg
  | ICmovz of arg * arg
  | ICmovnz of arg * arg
  | ICmovne of arg * arg
  | ICmove of arg * arg
  | ICmovge of arg * arg
  | ICmovg of arg * arg
  | ICmovle of arg * arg
  | ICmovl of arg * arg
  | ILea of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | IShl of arg * arg
  | IShr of arg * arg
  | ISar of arg * arg
  | IAnd of arg * arg
  | IOr of arg * arg
  | IXor of arg * arg
  | ILabel of string
  | IPush of arg
  | IPop of arg
  | ICall of arg
  | IRet
  | ICmp of arg * arg
  | ITest of arg * arg
  | IJo of arg
  | IJno of arg
  | IJe of arg
  | IJne of arg
  | IJl of arg
  | IJle of arg
  | IJg of arg
  | IJge of arg
  | IJmp of arg
  | IJz of arg
  | IJnz of arg
  | ILineComment of string
  | IInstrComment of instruction * string
[@@deriving variants, show {with_path= false}]

let rec string_of_arg (a : arg) : string =
  match a with
  | Const n -> sprintf "%Ld" n
  | HexConst n -> sprintf "0x%Lx" n
  | Reg r -> show_reg r
  | RegOffset (n, r) ->
      if n >= 0 then
        sprintf "[%s+%d]" (show_reg r) (n * word_size)
      else
        sprintf "[%s-%d]" (show_reg r) (-1 * n * word_size)
  | RegOffsetReg (r1, r2, mul, off) ->
      sprintf "[%s + %s * %d + %d]" (show_reg r1) (show_reg r2) mul off
  | Sized (size, a) ->
      sprintf "%s %s"
        ( match size with
        | QWORD_PTR -> "QWORD"
        | DWORD_PTR -> "DWORD"
        | WORD_PTR -> "WORD"
        | BYTE_PTR -> "BYTE" )
        (string_of_arg a)
  | Label s -> s
  | LabelContents s -> sprintf "[%s]" s
  | RelLabel s -> sprintf "[rel %s]" s
;;

let rec string_of_instruction =
  let unary_instr_fmt name arg = sprintf "  %s %s" name (string_of_arg arg) in
  let binary_instr_fmt name left right =
    sprintf "  %s %s, %s" name (string_of_arg left) (string_of_arg right)
  in
  function
  | IMov (d, v) -> binary_instr_fmt "mov" d v
  | ICmovz (d, v) -> binary_instr_fmt "cmovz" d v
  | ICmovnz (d, v) -> binary_instr_fmt "cmovnz" d v
  | ICmovne (d, v) -> binary_instr_fmt "cmovne" d v
  | ICmove (d, v) -> binary_instr_fmt "cmove" d v
  | ICmovge (d, v) -> binary_instr_fmt "cmovge" d v
  | ICmovg (d, v) -> binary_instr_fmt "cmovg" d v
  | ICmovle (d, v) -> binary_instr_fmt "cmovle" d v
  | ICmovl (d, v) -> binary_instr_fmt "cmovl" d v
  | ILea (d, v) -> binary_instr_fmt "lea" d v
  | IAdd (d, v) -> binary_instr_fmt "add" d v
  | ISub (d, v) -> binary_instr_fmt "sub" d v
  | IMul (d, v) -> binary_instr_fmt "imul" d v
  | ICmp (l, r) -> binary_instr_fmt "cmp" l r
  | ILabel name -> name ^ ":"
  | IJo l -> unary_instr_fmt "jo near" l
  | IJno l -> unary_instr_fmt "jno near" l
  | IJe l -> unary_instr_fmt "je near" l
  | IJne l -> unary_instr_fmt "jne near" l
  | IJl l -> unary_instr_fmt "jl near" l
  | IJle l -> unary_instr_fmt "jle near" l
  | IJg l -> unary_instr_fmt "jg near" l
  | IJge l -> unary_instr_fmt "jge near" l
  | IJmp l -> unary_instr_fmt "jmp near" l
  | IJz l -> unary_instr_fmt "jz near" l
  | IJnz l -> unary_instr_fmt "jnz near" l
  | IAnd (d, v) -> binary_instr_fmt "and" d v
  | IOr (d, v) -> binary_instr_fmt "or" d v
  | IXor (d, v) -> binary_instr_fmt "xor" d v
  | IShl (d, v) -> binary_instr_fmt "shl" d v
  | IShr (d, v) -> binary_instr_fmt "shr" d v
  | ISar (d, v) -> binary_instr_fmt "sar" d v
  | IPush v -> unary_instr_fmt "push" v
  | IPop d -> unary_instr_fmt "pop" d
  | ICall l -> unary_instr_fmt "call" l
  | IRet -> "  ret"
  | ITest (arg, comp) -> binary_instr_fmt "test" arg comp
  | ILineComment str -> sprintf "  ;; %s" str
  | IInstrComment (instr, str) -> sprintf "%s ; %s" (string_of_instruction instr) str
;;

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (string_of_instruction i)) "" is ^ "\n"
;;
