open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Utils

(* create aliases of modules for testing because dune is being wierd *)
module T = struct
  module ANF = ANF
  module Builtins = Builtins
  module Closures = Closures
  module Common = Common
  module Desugar = Desugar
  module DesugarCheck = DesugarCheck
  module Devalue = Devalue
  module Exprify = Exprify
  module Rename = Rename
  module WellFormed = WellFormed
end

let free_vars (e : 'a aexpr) : string list =
  let rec helpA seen = function
    | ALet (name, value, body, _) -> helpC (name :: seen) value @ helpA (name :: seen) body
    | ALetRec (bindings, body, _) ->
        let names, values = List.split bindings in
        List.concat_map (helpC (names @ seen)) values @ helpA (names @ seen) body
    | ACExpr c -> helpC seen c
  and helpC seen = function
    | CLambda (args, body, _) -> helpA (args @ seen) body
    | CImmExpr imm -> helpI seen imm
    | CPrim1 (_, arg, _) -> helpI seen arg
    | CPrim2 (_, l, r, _) -> helpI seen l @ helpI seen r
    | CIf (c, t, e, _) -> helpI seen c @ helpA seen t @ helpA seen e
    | CTuple (items, _) -> List.concat_map (helpI seen) items
    | CGetItem (tup, idx, _) -> helpI seen tup @ helpI seen idx
    | CSetItem (tup, idx, v, _) -> helpI seen tup @ helpI seen idx @ helpI seen v
    | CSeq (f, s, _) -> helpA seen f @ helpA seen s
    | CApp (_, args, Native, _) -> List.concat_map (helpI seen) args
    | CApp (fn, args, _, _) -> helpI seen fn @ List.concat_map (helpI seen) args
  and helpI seen = function
    | ImmId (id, _) -> if List.mem id seen then [] else [id]
    | _ -> []
  in
  helpA [] e |> List.sort_uniq Stdlib.compare
;;

let fst_tag_A x = get_tag_A x |> fst

let fst_tag_C x = get_tag_C x |> fst

let fst_tag_I x = get_tag_I x |> fst

let free_vars_cache (e : 'a aexpr) : (StringSet.t * 'a) aexpr =
  let rec helpA (seen : StringSet.t) = function
    | ALet (name, value, body, t) ->
        let value' = helpC seen value in
        let body' = helpA (StringSet.add name seen) body in
        let combined = StringSet.union (fst_tag_C value') (fst_tag_A body') in
        ALet (name, value', body', (StringSet.remove name combined, t))
    | ALetRec (bindings, body, t) ->
        let new_names = StringSet.of_list (List.map fst bindings) in
        let seen' = StringSet.union new_names seen in
        let bindings' = List.map (fun (n, x) -> (n, helpC seen' x)) bindings in
        let bindings_free = List.map (fun (_, x) -> fst_tag_C x) bindings' in
        let body' = helpA seen' body in
        let all_free = StringSet.union_all (fst_tag_A body' :: bindings_free) in
        ALetRec (bindings', body', (StringSet.diff all_free new_names, t))
    | ACExpr c -> ACExpr (helpC seen c)
  and helpC (seen : StringSet.t) = function
    | CLambda (args, body, t) ->
        let new_names = StringSet.of_list args in
        let body' = helpA (StringSet.union new_names seen) body in
        CLambda (args, body', (StringSet.diff (fst_tag_A body') new_names, t))
    | CImmExpr imm -> CImmExpr (helpI seen imm)
    | CPrim1 (op, arg, t) ->
        let arg' = helpI seen arg in
        CPrim1 (op, arg', (fst_tag_I arg', t))
    | CPrim2 (op, l, r, t) ->
        let l' = helpI seen l in
        let r' = helpI seen r in
        CPrim2 (op, l', r', (StringSet.union (fst_tag_I l') (fst_tag_I r'), t))
    | CIf (c, thn, els, t) ->
        let c' = helpI seen c in
        let t' = helpA seen thn in
        let e' = helpA seen els in
        CIf (c', t', e', (StringSet.union3 (fst_tag_I c') (fst_tag_A t') (fst_tag_A e'), t))
    | CTuple (items, t) ->
        let items' = List.map (helpI seen) items in
        let tag' = (StringSet.union_all (List.map fst_tag_I items'), t) in
        CTuple (items', tag')
    | CGetItem (tup, idx, t) ->
        let tup' = helpI seen tup in
        let idx' = helpI seen idx in
        CGetItem (tup', idx', (StringSet.union (fst_tag_I tup') (fst_tag_I idx'), t))
    | CSetItem (tup, idx, v, t) ->
        let tup' = helpI seen tup in
        let idx' = helpI seen idx in
        let v' = helpI seen v in
        CSetItem
          (tup', idx', v', (StringSet.union3 (fst_tag_I tup') (fst_tag_I idx') (fst_tag_I v'), t))
    | CSeq (f, s, t) ->
        let f' = helpA seen f in
        let s' = helpA seen s in
        CSeq (f', s', (StringSet.union (fst_tag_A f') (fst_tag_A s'), t))
    | CApp (ImmId (fn, fn_t), args, Native, t) ->
        (* for a native function call, the free variable set does not actually matter
        and we can lie about it. we simply call into C with thee _actually the
        identifier_. *)
        let fn' = ImmId (fn, (StringSet.empty, fn_t)) in
        let args' = List.map (helpI seen) args in
        CApp (fn', args', Native, (StringSet.union_all (List.map fst_tag_I args'), t))
    | CApp (fn, args, call_type, t) ->
        let fn' = helpI seen fn in
        let args' = List.map (helpI seen) args in
        CApp
          ( fn',
            args',
            call_type,
            (StringSet.union (fst_tag_I fn') (StringSet.union_all (List.map fst_tag_I args')), t) )
  and helpI (_ : StringSet.t) = function
    | ImmId (id, t) -> ImmId (id, (StringSet.singleton id, t))
    | imm -> map_atag_I (fun t -> (StringSet.empty, t)) imm
  in
  helpA StringSet.empty e
;;

(* Returns the stack-index (in words) of the deepest stack index used for any
   of the variables in this expression *)
let deepest_stack (e : tag aexpr) (env : arg name_envt tag_envt) =
  let rec helpA curr_tag e =
    match e with
    | ALet (name, value, body, _) ->
        List.fold_left max 0
          [name_to_offset curr_tag name; helpC curr_tag value; helpA curr_tag body]
    | ALetRec (bindings, body, _) ->
        List.fold_left max (helpA curr_tag body)
          (List.map (fun (name, _) -> name_to_offset curr_tag name) bindings)
    | ACExpr e -> helpC curr_tag e
  and helpC curr_tag e =
    match e with
    | CSeq (first, rest, _) -> max (helpA curr_tag first) (helpA curr_tag rest)
    | CIf (c, t, f, _) -> List.fold_left max 0 [helpI curr_tag c; helpA curr_tag t; helpA curr_tag f]
    | CPrim1 (_, i, _) -> helpI curr_tag i
    | CPrim2 (_, i1, i2, _) -> max (helpI curr_tag i1) (helpI curr_tag i2)
    | CApp (_, args, Native, _) -> max 0 (List.fold_left max 0 (List.map (helpI curr_tag) args))
    | CApp (func, args, _, _) ->
        max (helpI curr_tag func) (List.fold_left max 0 (List.map (helpI curr_tag) args))
    | CTuple (vals, _) -> List.fold_left max 0 (List.map (helpI curr_tag) vals)
    | CGetItem (t, _, _) -> helpI curr_tag t
    | CSetItem (t, _, v, _) -> max (helpI curr_tag t) (helpI curr_tag v)
    | CLambda (args, body, tag) ->
        let args_free =
          List.map (fun name -> name_to_offset tag name) (args @ free_vars (ACExpr e))
        in
        max (List.fold_left max 0 args_free) (helpA tag body)
    | CImmExpr i -> helpI curr_tag i
  and helpI curr_tag i =
    match i with
    | ImmNil _ -> 0
    | ImmNum _ -> 0
    | ImmBool _ -> 0
    | ImmId (name, _) -> name_to_offset curr_tag name
  and name_to_offset t name =
    match lookup env t name with
    | RegOffset (words, RBP) -> words / -1 (* negative because stack direction *)
    | _ -> 0
  in
  max
    (helpA (get_tag_A e) e)
    0 (* if only parameters are used, helpA might return a negative value *)
;;

let stack_allocation (e : (StringSet.t * tag) aexpr) : tag aexpr * arg name_envt tag_envt =
  let rec helpC env si curr_tag e =
    match e with
    | CImmExpr _ | CPrim1 _ | CPrim2 _ | CApp _ | CTuple _ | CGetItem _ | CSetItem _ -> env
    | CIf (_, t, e, _) -> helpA env si curr_tag t @ helpA env si curr_tag e @ env
    | CSeq (f, s, _) -> helpA env si curr_tag f @ helpA env si curr_tag s @ env
    | CLambda (args, body, tag) as l ->
        let num_args = List.length args in
        let arg_env = List.mapi (fun i name -> (tag, (name, RegOffset (~-(i + 1), RBP)))) args in
        let free_env =
          List.mapi
            (fun i name -> (tag, (name, RegOffset (~-(num_args + i + 1), RBP))))
            (free_vars (ACExpr l))
        in
        let body_env = helpA env ~-(num_args + List.length free_env + 1) tag body in
        arg_env @ free_env @ body_env @ env
  and helpA env si curr_tag e =
    match e with
    | ACExpr c -> helpC env si curr_tag c
    | ALet (name, value, body, _) ->
        ((curr_tag, (name, RegOffset (si, RBP))) :: helpC env (si - 1) curr_tag value)
        @ helpA env (si - 1) curr_tag body
        @ env
    | ALetRec (bindings, body, _) ->
        List.mapi (fun i (name, _) -> (curr_tag, (name, RegOffset (si - i, RBP)))) bindings
        (* we can make up a garbage tag here because we know the right-hand sides of let rec
             will always be a lambda, which will use its own tag anyway. *)
        @ List.concat_map (fun (_, value) -> helpC env si 0 value) bindings
        @ helpA env (si - List.length bindings) curr_tag body
  in
  (map_atag_A snd e, helpA [] (-1) (get_tag_A e |> snd) (map_atag_A snd e) |> ungroup)
;;

(** Eliminates dead variables from the program:
    - Converts ALet to CSeq when the bound variable is never used
    - Removes unused bindings from ALetRec
    This assumes the expressions have already been annotated with free variables. *)
let eliminate_dead_vars (e : (StringSet.t * tag) aexpr) : (StringSet.t * tag) aexpr =
  let rec helpA e =
    match e with
    | ACExpr c -> ACExpr (helpC c)
    | ALet (name, value, body, tag) ->
        let body_free = fst_tag_A body in
        if StringSet.mem name body_free then
          ALet (name, helpC value, helpA body, tag)
        else
          ACExpr (CSeq (ACExpr (helpC value), helpA body, tag))
    | ALetRec (bindings, body, tag) ->
        let body_free = fst_tag_A body in
        let binding_names = List.map fst bindings |> StringSet.of_list in
        let binding_graph =
          List.map
            (fun (name, expr) -> (name, StringSet.inter (fst_tag_C expr) binding_names))
            bindings
        in
        let rec find_used_vars used_so_far =
          let new_used =
            List.fold_left
              (fun acc (name, used_in_expr) ->
                if StringSet.mem name used_so_far then
                  StringSet.union acc used_in_expr
                else
                  acc )
              StringSet.empty binding_graph
          in
          let combined = StringSet.union used_so_far new_used in
          if StringSet.equal combined used_so_far then
            used_so_far
          else
            find_used_vars combined
        in
        let used_vars = find_used_vars (StringSet.inter body_free binding_names) in
        let live_bindings = List.filter (fun (name, _) -> StringSet.mem name used_vars) bindings in
        let processed_bindings =
          List.map (fun (name, value) -> (name, helpC value)) live_bindings
        in
        if List.is_empty processed_bindings then
          helpA body
        else
          ALetRec (processed_bindings, helpA body, tag)
  and helpC e =
    match e with
    | CLambda (args, body, tag) -> CLambda (args, helpA body, tag)
    | CIf (cond, thn, els, tag) -> CIf (cond, helpA thn, helpA els, tag)
    | CSeq (first, second, tag) -> CSeq (helpA first, helpA second, tag)
    | CPrim1 _ | CPrim2 _ | CApp _ | CImmExpr _ | CTuple _ | CGetItem _ | CSetItem _ -> e
  in
  helpA e
;;

module Compile = struct
  open Common

  let heap_align_to_16_bytes needed = needed + ((16 - (needed mod 16)) mod 16)

  (** returns the requirement in BYTES *)
  let heap_bytes_for_closure free = (3 + free) * word_size

  let call_c location args =
    let reg_args = take_up_to 6 args in
    let stack_args = drop_up_to 6 args in
    let needs_padding = List.length stack_args mod 2 <> 0 in
    let stack_args = if needs_padding then stack_args @ [Const stack_padding] else stack_args in
    List.map2 imov
      ( Regs.first_six_args
      |> Array.to_list
      |> List.filteri (fun i _ -> i < List.length reg_args)
      |> List.map reg )
      reg_args
    @ (List.concat_map (fun arg -> [IMov (Reg RAX, arg); IPush (Reg RAX)]) stack_args |> List.rev)
    @ [ICall location; IAdd (Reg RSP, Const (Int64.of_int (List.length stack_args * word_size)))]
  ;;

  let reserve words tag =
    let ok = tagged_symbol "$memcheck" tag in
    [ IInstrComment (IMov (Reg RAX, RelLabel "HEAP_END"), sprintf "Reserving %d words" words);
      ISub (Reg RAX, Const (Int64.of_int (words * word_size)));
      ICmp (Reg RAX, Reg Regs.heap);
      IJge (Label ok) ]
    @ call_c (Label "try_gc")
        [ Sized (QWORD_PTR, Reg Regs.heap);
          (* alloc_ptr in C *)
          Sized (QWORD_PTR, Const (Int64.of_int (words * word_size)));
          (* bytes_needed in C *)
          Sized (QWORD_PTR, Reg RBP);
          (* first_frame in C *)
          Sized (QWORD_PTR, Reg RSP) (* stack_top in C *) ]
    @ [ IInstrComment
          ( IMov (Reg Regs.heap, Reg RAX),
            "assume gc success if returning here, so RAX holds the new heap_reg value" );
        ILabel ok ]
  ;;

  let rec compile_fun (fun_name : string) args body (env : arg name_envt tag_envt) (t : tag) :
      instruction list =
    (* closure argument is already in the args *)
    let num_args = List.length args in
    let arg_offset_bytes = ~-word_size * num_args in
    let free = free_vars (ACExpr (CLambda (args, body, t))) in
    let deepest_local_offset =
      deepest_stack (ACExpr (CLambda (args, body, t))) env + List.length free
    in
    let deepest_local_offset_bytes = word_size * deepest_local_offset in
    let relative_max_offset_bytes = deepest_local_offset_bytes - arg_offset_bytes in
    let padding = if deepest_local_offset mod 2 = 0 then 1 else 0 in
    let padding_bytes = padding * word_size in
    let prelude =
      List.init (relative_max_offset_bytes / word_size) (fun i ->
          push_big ~move:(i == 0) snake_uninit "unninit" )
      @ List.init padding (fun _ -> push_big stack_padding "padding")
      |> List.flatten
    in
    let unfree =
      if List.is_empty free then
        []
      else
        [ IMov (Reg RAX, RegOffset (-1, RBP));
          ISub (Reg RAX, Const SnakeTags.closure_tag);
          IInstrComment (IAdd (Reg RAX, Const (Int64.of_int (3 * word_size))), "closure free offset")
        ]
        @ List.concat_map
            (fun f ->
              [ IInstrComment (IMov (Reg Regs.scratch, RegOffset (0, RAX)), "unfree: " ^ f);
                IMov (Sized (QWORD_PTR, lookup env t f), Reg Regs.scratch);
                IAdd (Reg RAX, Const (Int64.of_int word_size)) ] )
            free
    in
    let postlude =
      [ IInstrComment
          ( IAdd (Reg RSP, Const (Int64.of_int (relative_max_offset_bytes + padding_bytes))),
            "start fun postlude" ) ]
      @ List.map (fun x -> IInstrComment (IPop (Reg Regs.scratch), "pop: " ^ x)) args
      @ [IPop (Reg RBP); IInstrComment (IRet, "end fun postlude")]
    in
    [ILabel fun_name]
    @ prelude
    @ unfree
    @ compile_aexpr body env t (List.length args) true
    @ postlude

  and compile_aexpr
      (e : tag aexpr)
      (env : arg name_envt tag_envt)
      (t : tag)
      (num_args : int)
      (is_tail : bool) : instruction list =
    match e with
    | ACExpr c ->
        [ILineComment ("start " ^ string_of_cexpr c)]
        @ compile_cexpr c env t num_args is_tail
        @ [ILineComment ("end " ^ string_of_cexpr c)]
    | ALet (name, value, body, _) ->
        [ILineComment ("(let " ^ name ^ ") start " ^ string_of_cexpr value)]
        @ compile_cexpr value env t num_args false
        @ [ILineComment ("(let " ^ name ^ ") end " ^ string_of_cexpr value)]
        @ [IMov (lookup env t name, Reg RAX)]
        @ compile_aexpr body env t num_args is_tail
    | ALetRec ([], _, _) -> ice "wtf empty let rec"
    | ALetRec (bindings, body, tag) ->
        let infos =
          List.map
            (fun (name, lam) -> (name, lam, free_vars (ACExpr lam), lookup env t name))
            bindings
        in
        let heap_size =
          List.fold_left
            (fun a (_, _, f, _) ->
              a + heap_align_to_16_bytes (heap_bytes_for_closure (List.length f)) )
            0 infos
        in
        [ ILineComment
            (sprintf "(let rec %s) start (%s)"
               ([%show: string list] (List.map fst bindings))
               (string_of_aexpr body) ) ]
        @ reserve (heap_size / word_size) tag
        @ [ILineComment "==== [let rec]: lambdas ===="]
        @ List.concat_map
            (fun (name, l, _, _) ->
              compile_cexpr l env t num_args is_tail ~do_reserve_lambda:false
              @ [ IInstrComment
                    (IMov (Sized (QWORD_PTR, lookup env t name), Reg RAX), "let rec lambda: " ^ name)
                ] )
            infos
        @ [ILineComment "==== [let rec]: free vars ===="]
        @ List.concat_map
            (fun (name, _, f, l) ->
              [ IInstrComment (IMov (Reg Regs.scratch, l), name);
                ISub (Reg Regs.scratch, Const SnakeTags.closure_tag);
                IInstrComment
                  ( IAdd (Reg Regs.scratch, Const (Int64.of_int (3 * word_size))),
                    "go to first closed" ) ]
              @ List.flatten
              @@ List.mapi
                   (fun i v ->
                     [ IMov (Reg Regs.scratch2, lookup env t v);
                       IMov (Sized (QWORD_PTR, RegOffset (i, Regs.scratch)), Reg Regs.scratch2) ] )
                   f )
            (List.filter (fun (_, _, f, _) -> not (List.is_empty f)) infos)
        @ [ILineComment "==== [let rec]: body ===="]
        @ compile_aexpr body env t num_args is_tail
        @ [ ILineComment
              (sprintf "(let rec %s) end (%s)"
                 ([%show: string list] (List.map fst bindings))
                 (string_of_aexpr body) ) ]

  (** Compile a CExpr given its arg envirment, the number of args of the current
    function and whether it is in tail position. *)
  and compile_cexpr
      ?(do_reserve_lambda = true)
      (e : tag cexpr)
      (env : arg name_envt tag_envt)
      (t : tag)
      (num_args : int)
      (is_tail : bool) =
    let type_check (mask, tag) name reg err =
      [ IInstrComment (IMov (Reg Regs.scratch2, Const mask), "start " ^ name ^ " typecheck");
        IAnd (Reg Regs.scratch2, Reg reg);
        ICmp (Reg Regs.scratch2, Const tag);
        ICmovne (Reg RAX, Reg reg);
        IInstrComment (IJne (Label err), "end " ^ name ^ " typecheck") ]
    in
    let deref_check =
      [ ICmp (Reg RAX, SnakeVal.nil);
        IJe (Label "error_nil_deref");
        ICmp (Reg Regs.scratch, Const 0L);
        IJl (Label "error_idx_negative");
        IInstrComment (ISub (Reg RAX, HexConst 0x1L), "remove tuple tag");
        ICmp (Reg Regs.scratch, RegOffset (0, RAX));
        IJge (Label "error_idx_high") ]
    in
    let check_num = type_check (SnakeTags.num_tag_mask, SnakeTags.num_tag) "num" in
    let check_bool = type_check (SnakeTags.bool_tag_mask, SnakeTags.bool_tag) "bool" in
    let check_tup = type_check (SnakeTags.tup_tag_mask, SnakeTags.tup_tag) "tuple" in
    let check_closure = type_check (SnakeTags.closure_tag_mask, SnakeTags.closure_tag) "closure" in
    let arithmetic inst rhs = [inst (Reg RAX) rhs; IJo (Label "error_overflow")] in
    match e with
    | CImmExpr imm -> [mov_imm_with_src imm env t RAX "Immediate "]
    | CPrim1 (op, arg, _) ->
        let type_check =
          match op with
          | Add1 | Sub1 -> check_num RAX "error_arith_not_num"
          | Not -> check_bool RAX "error_logic_not_bool"
          | _ -> []
        in
        let is_type mask tag name =
          [ IInstrComment (IAnd (Reg RAX, HexConst mask), sprintf "start is %s check" name);
            ICmp (Reg RAX, HexConst tag);
            IMov (Reg Regs.scratch2, SnakeVal.s_true);
            ICmove (Reg RAX, Reg Regs.scratch2);
            IMov (Reg Regs.scratch2, SnakeVal.s_false);
            IInstrComment (ICmovne (Reg RAX, Reg Regs.scratch2), sprintf "end is %s check" name) ]
        in
        let operation =
          match op with
          | Add1 -> arithmetic iadd SnakeVal.one
          | Sub1 -> arithmetic isub SnakeVal.one
          | Not ->
              [IMov (Reg Regs.scratch, SnakeTags.book_val_mask); IXor (Reg RAX, Reg Regs.scratch)]
          | IsNum -> is_type SnakeTags.num_tag_mask SnakeTags.num_tag "num"
          | IsBool -> is_type SnakeTags.bool_tag_mask SnakeTags.bool_tag "bool"
          | IsTuple -> is_type SnakeTags.tup_tag_mask SnakeTags.tup_tag "tuple"
          | Print -> call_c (Label "print") [Reg RAX]
          | PrintStack ->
              call_c (Label "print_stack") [Reg RAX; Reg RSP; Reg RBP; Const (Int64.of_int num_args)]
        in
        [mov_imm_with_src arg env t RAX "prim1 arg "] @ type_check @ operation
    | CPrim2 (CheckSize, l, r, _) ->
        (* this is internal only, and we can assume safely that the right hand side will be number *)
        [ mov_imm_with_src l env t RAX "size left ";
          mov_imm_with_src r env t Regs.scratch "size right " ]
        @ check_tup RAX "error_destruct_not_tup"
        @ [ IMov (Reg Regs.scratch2, Reg RAX);
            ISub (Reg Regs.scratch2, Const SnakeTags.tup_tag);
            IMov (Reg Regs.scratch2, RegOffset (0, Regs.scratch2));
            ICmp (Reg Regs.scratch2, Reg Regs.scratch);
            IJne (Label "error_destruct_size") ]
    | CPrim2 (op, l, r, _) ->
        let type_check reg =
          match op with
          | Plus | Minus | Times -> check_num reg "error_arith_not_num"
          | Greater | GreaterEq | Less | LessEq -> check_num reg "error_comp_not_num"
          | And | Or -> ice "compile_expr: logic should be desurgared"
          | Eq -> []
          | CheckSize -> ice "checksize implemented previously"
        in
        let comparison false_cond true_cond =
          [ IInstrComment (ICmp (Reg RAX, Reg Regs.scratch), "start comparison");
            IMov (Reg Regs.scratch, SnakeVal.s_false);
            false_cond (Reg RAX) (Reg Regs.scratch);
            IMov (Reg Regs.scratch, SnakeVal.s_true);
            IInstrComment (true_cond (Reg RAX) (Reg Regs.scratch), "end comparison") ]
        in
        let operation =
          match op with
          | Plus -> arithmetic iadd (Reg Regs.scratch)
          | Minus -> arithmetic isub (Reg Regs.scratch)
          | Times -> arithmetic imul (Reg Regs.scratch) @ [ISar (Reg RAX, Const 1L)]
          | Greater -> comparison icmovle icmovg
          | GreaterEq -> comparison icmovl icmovge
          | Less -> comparison icmovge icmovl
          | LessEq -> comparison icmovg icmovle
          | Eq -> comparison icmovne icmove
          | And | Or -> ice "compile_expr: logic should be desurgared"
          | _ -> ice "checksize implemented previously"
        in
        [mov_imm_with_src l env t RAX "prim2 left "]
        @ type_check RAX
        @ [mov_imm_with_src r env t Regs.scratch "prim2 right "]
        @ type_check Regs.scratch
        @ operation
    | CIf (c, thn, els, tag) ->
        (* the then label is never jumped to, but helps visually identify the sections *)
        let if_then_label = tagged_symbol "if_then" tag in
        let if_else_label = tagged_symbol "if_else" tag in
        let if_done_label = tagged_symbol "if_done" tag in
        [mov_imm_with_src c env t RAX "if cond "]
        @ check_bool RAX "error_if_not_bool"
        @ [ IInstrComment (IMov (Reg Regs.scratch, SnakeVal.s_false), "if check");
            ICmp (Reg RAX, Reg Regs.scratch);
            IJe (Label if_else_label);
            ILabel if_then_label ]
        @ compile_aexpr thn env t num_args is_tail
        @ [IJmp (Label if_done_label); ILabel if_else_label]
        @ compile_aexpr els env t num_args is_tail
        @ [ILabel if_done_label]
    | CApp (fn, args, typ, tag) -> (
        let arity_check =
          [ ISub (Reg RAX, Const SnakeTags.closure_tag);
            IMov (Reg Regs.scratch, RegOffset (0, RAX));
            ICmp (Reg Regs.scratch, Const (snake_of_int (List.length args)));
            IMov (Reg Regs.scratch, Const (snake_of_int (List.length args)));
            IJne (Label "error_call_arity") ]
        in
        let compile_snake_fun () =
          let pre_check =
            [mov_imm_with_src fn env t RAX "app snake fn "]
            @ check_closure RAX "error_call_not_fn"
            @ arity_check
          in
          let args_stack =
            args
            |> List.mapi (fun i x -> (compile_imm x env t, i))
            |> List.concat_map (fun ((x, c), i) ->
                   [ IInstrComment
                       (IMov (Reg Regs.scratch2, Sized (QWORD_PTR, x)), sprintf "arg%d %s" (i + 1) c);
                     IPush (Reg Regs.scratch2) ]
                   |> if is_tail then List.rev else Fun.id )
          in
          if is_tail then
            let surplus = max 0 (List.length args - num_args) in
            pre_check
            @ List.init surplus (Fun.const (IPush (Const 0L)))
            @ (args_stack |> List.rev)
            @ List.mapi (fun i _ -> IPop (Sized (QWORD_PTR, RegOffset ((1 + i) * -1, RBP)))) args
            @ [ IMov (Reg Regs.scratch, Reg RBP);
                IAdd (Reg Regs.scratch, Const (Int64.of_int (List.length args * -word_size)));
                IMov (Reg RSP, Reg Regs.scratch) ]
            @ [IJmp (RegOffset (1, RAX))]
          else
            let ret_label = tagged_symbol "app_ret" tag in
            pre_check
            @ [ ILea (Reg Regs.scratch, RelLabel ret_label);
                IPush (Reg Regs.scratch);
                IPush (Reg RBP);
                IMov (Reg Regs.scratch, Reg RSP) ]
            @ args_stack
            @ [IMov (Reg RBP, Reg Regs.scratch); IJmp (RegOffset (1, RAX))]
            @ [ILabel ret_label]
        in
        match typ with
        | Native ->
            let fn_name =
              match fn with
              | ImmId (name, _) -> name
              | _ -> ice "native function application: expected identifier"
            in
            (* TODO: add comments for call_c??? *)
            call_c (Label fn_name) (List.map (fun x -> fst @@ compile_imm x env t) args)
        | Snake -> compile_snake_fun ()
        | Check ->
            let post_check_label = tagged_symbol "post_check_block" tag in
            [ILea (Reg Regs.scratch, RelLabel post_check_label)]
            @ call_c (Label "start_check_block") [Reg RBP; Reg RSP; Reg Regs.scratch]
            @ compile_snake_fun ()
            @ call_c (Label "end_check_block") []
            @ [ILabel post_check_label]
            @ [IMov (Reg RAX, SnakeVal.nil)]
            (* RAX could have anything in it at this point, we will discard it anyways *)
        | _ -> ice "compile_cexpr: unsupported call type" )
    | CSeq (first, second, _) ->
        compile_aexpr first env t num_args false @ compile_aexpr second env t num_args is_tail
    | CTuple (items, tag) ->
        let item_exprs = List.map (fun e -> compile_imm e env t) items in
        let tup_length = List.length items in
        let misaligned = tup_length mod 2 = 0 in
        let all = item_exprs @ if misaligned then [(HexConst 0x0L, "padding")] else [] in
        (* adding 1 to also include the length word *)
        let words_to_skip = List.length all + 1 in
        reserve words_to_skip tag
        @ [ IInstrComment
              ( IMov (Reg Regs.scratch, Const (snake_of_int tup_length)),
                sprintf "tuple of length %d" tup_length );
            IMov (Sized (QWORD_PTR, RegOffset (0, R15)), Reg Regs.scratch) ]
        @ ( List.mapi
              (fun i (item, comm) ->
                [ IInstrComment (IMov (Reg RAX, item), "tuple " ^ comm);
                  IMov (Sized (QWORD_PTR, RegOffset (i + 1, Regs.heap)), Reg RAX) ] )
              all
          |> List.flatten )
        @ [ IMov (Reg RAX, Reg Regs.heap);
            IInstrComment (IAdd (Reg RAX, Const SnakeTags.tup_tag), "add tuple tag");
            IAdd (Reg Regs.heap, Const (Int64.of_int (words_to_skip * word_size))) ]
    | CGetItem (tup, idx, _) ->
        [ mov_imm_with_src tup env t RAX "get item tup ";
          mov_imm_with_src idx env t Regs.scratch "get item idx " ]
        @ check_tup RAX "error_not_tuple"
        @ check_num Regs.scratch "error_idx_not_num"
        @ deref_check
        @ [ ISar (Reg Regs.scratch, Const 1L);
            IMov (Reg RAX, RegOffsetReg (RAX, Regs.scratch, word_size, word_size)) ]
    | CSetItem (tup, idx, v, _) ->
        [ mov_imm_with_src tup env t RAX "set item tup ";
          mov_imm_with_src idx env t Regs.scratch "set item idx " ]
        @ check_tup RAX "error_not_tuple"
        @ check_num Regs.scratch "error_idx_not_num"
        @ deref_check
        @ [ mov_imm_with_src v env t Regs.scratch2 "set item val ";
            ISar (Reg Regs.scratch, Const 1L);
            IMov (RegOffsetReg (RAX, Regs.scratch, word_size, word_size), Reg Regs.scratch2);
            IMov (Reg RAX, Reg Regs.scratch2) ]
    | CLambda (args, body, tag) as e ->
        let free = free_vars (ACExpr e) |> List.sort String.compare in
        let start_label = tagged_symbol "lam" tag in
        let end_label = tagged_symbol "lam_end" tag in
        let total = heap_align_to_16_bytes (heap_bytes_for_closure (List.length free)) in
        [ILineComment ("[lambda start]: " ^ string_of_cexpr e)]
        @ (if do_reserve_lambda then reserve (total / word_size) tag else [])
        @ [ IInstrComment
              ( IMov
                  ( Sized (QWORD_PTR, RegOffset (0, Regs.heap)),
                    Const (snake_of_int (List.length args)) ),
                sprintf "lambda arity: %d" (List.length args) );
            ILea (Reg Regs.scratch, RelLabel start_label);
            IMov (Sized (QWORD_PTR, RegOffset (1, Regs.heap)), Reg Regs.scratch);
            IInstrComment
              ( IMov
                  ( Sized (QWORD_PTR, RegOffset (2, Regs.heap)),
                    Const (snake_of_int (List.length free)) ),
                sprintf "capturing %d free variables" (List.length free) ) ]
        @ [ILineComment "populate free variables: "]
        @ ( List.mapi
              (fun i name ->
                [ IInstrComment (IMov (Reg RAX, lookup env t name), "copy " ^ name);
                  IMov (Sized (QWORD_PTR, RegOffset (i + 3, Regs.heap)), Reg RAX) ] )
              free
          |> List.concat )
        @ [ IMov (Reg RAX, Reg Regs.heap);
            IAdd (Reg Regs.heap, Const (Int64.of_int (heap_align_to_16_bytes total)));
            IAdd (Reg RAX, Const SnakeTags.closure_tag);
            IJmp (Label end_label) ]
        @ compile_fun start_label args body env tag
        @ [ILabel end_label]
        @ [ILineComment ("[lambda end]: " ^ string_of_cexpr e)]

  and compile_imm e env t =
    match e with
    | ImmId (x, _) -> (lookup env t x, x)
    | ImmNum (n, _) -> (Const (Int64.shift_left n 1), Int64.to_string n)
    | ImmBool (true, _) -> (SnakeVal.s_true, "true")
    | ImmBool (false, _) -> (SnakeVal.s_false, "false")
    | ImmNil _ -> (SnakeVal.nil, "nil")

  and mov_imm_with_src imm env t reg comm =
    let intr, src = compile_imm imm env t in
    IInstrComment (IMov (Reg reg, intr), comm ^ src)
  ;;

  let compile_prog ((anfed : tag aexpr), (env : arg name_envt tag_envt)) : string =
    let directives =
      [ "section .text";
        "extern error";
        "extern print";
        "extern print_stack";
        "extern input";
        "extern equal";
        "extern try_gc";
        "extern set_stack_bottom";
        "extern start_check_block";
        "extern init_test";
        "extern test";
        "extern end_check_block";
        "extern HEAP_END";
        "global our_code_starts_here" ]
    in
    let create_error ?(inject = []) name code =
      [ILabel name] @ inject
      @ [ IMov (Reg Regs.first_six_args.(0), Reg RAX);
          IMov (Reg Regs.first_six_args.(1), Reg Regs.scratch);
          IMov (Reg Regs.first_six_args.(2), Const code);
          ICall (Label "error") ]
    in
    let errors =
      []
      @ create_error "error_comp_not_num" SnakeErrors.comp_not_num
      @ create_error "error_arith_not_num" SnakeErrors.arith_not_num
      @ create_error "error_logic_not_bool" SnakeErrors.logic_not_bool
      @ create_error "error_if_not_bool" SnakeErrors.if_not_bool
      @ create_error "error_overflow" SnakeErrors.overflow
      @ create_error "error_not_tuple" SnakeErrors.not_tuple
      @ create_error "error_idx_not_num" SnakeErrors.idx_not_num
      (* multiplying here to convert machine ints and pointers to snakeval, for proper messaging *)
      @ create_error "error_idx_high" ~inject:[IAdd (Reg RAX, HexConst 1L)] SnakeErrors.index_high
      @ create_error "error_idx_negative" SnakeErrors.idx_negative
      @ create_error "error_nil_deref" SnakeErrors.nil_deref
      @ create_error "error_oom" SnakeErrors.oom
      @ create_error "error_call_not_fn" SnakeErrors.call_not_fn
      @ create_error "error_call_arity"
          ~inject:[IAdd (Reg RAX, Const SnakeTags.closure_tag)]
          SnakeErrors.call_arity
      @ create_error "error_destruct_not_tup" SnakeErrors.destruct_not_tup
      @ create_error "error_destruct_size" SnakeErrors.destruct_size
    in
    let ocsh =
      let deepest_local_offset = deepest_stack anfed env in
      let padding = if deepest_local_offset mod 2 = 0 then 1 else 0 in
      let prelude =
        List.init deepest_local_offset (fun i -> push_big ~move:(i == 0) snake_uninit "unninit")
        @ List.init padding (fun _ -> push_big stack_padding "padding")
        |> List.flatten
      in
      [ILabel "our_code_starts_here"]
      @ [ IInstrComment
            ( IMov (Sized (QWORD_PTR, Reg Regs.heap), Reg Regs.first_six_args.(0)),
              "Load heap_reg with our argument, the aligned heap pointer" ) ]
      @ [IPush (Reg RBP); IMov (Reg RBP, Reg RSP)]
      @ call_c (Label "set_stack_bottom") [Reg RBP]
      @ prelude
      @ compile_aexpr anfed env (get_tag_A anfed) 0 false
      @ [ IInstrComment
            ( IAdd (Reg RSP, Const (Int64.of_int ((deepest_local_offset + padding) * word_size))),
              "start ocsh postlude" );
          IMov (Reg RSP, Reg RBP);
          IPop (Reg RBP);
          IInstrComment (IRet, "end ocsh postlude") ]
    in
    (directives |> String.concat "\n") ^ to_asm (ocsh @ errors)
  ;;
end

let run_if should_run f = if should_run then f else no_op_phase

let pick_alloc_strategy (strat : alloc_strategy) =
  match strat with
  | Naive -> stack_allocation
;;

let compile_to_string
    ?(no_builtins = false)
    ?(dont_elim_dead = false)
    (alloc_strat : alloc_strategy)
    (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> add_phase exprified Exprify.exprify
  |> add_phase devalued Devalue.devalue
  |> add_err_phase well_formed WellFormed.is_well_formed
  |> add_phase desugar_check DesugarCheck.desugar
  |> add_phase tagged tag
  |> run_if (not no_builtins) (add_phase injected Builtins.inject_builtins)
  |> add_phase desugared Desugar.desugar
  |> add_phase renamed Rename.rename_and_tag
  |> add_phase anfed (fun p -> atag (ANF.anf p))
  |> add_phase closed Closures.closure_conversion
  |> add_phase free_cached free_vars_cache
  |> run_if (not dont_elim_dead) (add_phase dead_vars_eliminated eliminate_dead_vars)
  |> add_phase locate_bindings (pick_alloc_strategy alloc_strat)
  |> add_phase result Compile.compile_prog
;;
