open Exprs
open Utils
open Printf
open Pretty

type 'a anf_bind =
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list

let anf (p : tag expr) : unit aexpr =
  let rec process_bind_let_rec (bind, rhs, _) =
    match bind with
    | BName (name, _, _) -> (name, helpC rhs)
    | _ ->
        ice
          (sprintf "Encountered a non-simple binding in ANFing a let-rec: %s" (string_of_bind bind))
  and process_bind_lambda bind =
    match bind with
    | BName (name, _, _) -> name
    | _ ->
        ice
          (sprintf "Encountered a non-simple binding in ANFing a lambda: %s" (string_of_bind bind))
  and helpC (e : tag expr) : unit cexpr * unit anf_bind list =
    match e with
    | EPrim1 (op, arg, _) ->
        let arg_imm, arg_setup = helpI arg in
        (CPrim1 (op, arg_imm, ()), arg_setup)
    | EPrim2 (op, left, right, _) ->
        let left_imm, left_setup = helpI left in
        let right_imm, right_setup = helpI right in
        (CPrim2 (op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf (cond, _then, _else, _) ->
        let cond_imm, cond_setup = helpI cond in
        (CIf (cond_imm, helpA _then, helpA _else, ()), cond_setup)
    | ELet ([(BName (name, _, _), value, _)], body, _) ->
        let val_ans, val_ctx = helpC value in
        let body_ans, body_ctx = helpC body in
        (body_ans, val_ctx @ [BLet (name, val_ans)] @ body_ctx)
    | ELet _ -> ice "let must be fully desugared before ANF"
    | ELambda (args, body, _) -> (CLambda (List.map process_bind_lambda args, helpA body, ()), [])
    | EApp (func, args, native, _) ->
        let func_ans, func_setup = helpI func in
        let new_args, new_setup = List.split (List.map helpI args) in
        (CApp (func_ans, new_args, native, ()), func_setup @ List.concat new_setup)
    | ESeq (first, second, _) ->
        let first = helpA first in
        let second = helpA second in
        (CSeq (first, second, ()), [])
    | ETuple (args, _) ->
        let new_args, new_setup = List.split (List.map helpI args) in
        (CTuple (new_args, ()), List.concat new_setup)
    | EGetItem (tup, idx, _) ->
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        (CGetItem (tup_imm, idx_imm, ()), tup_setup @ idx_setup)
    | ESetItem (tup, idx, newval, _) ->
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        let new_imm, new_setup = helpI newval in
        (CSetItem (tup_imm, idx_imm, new_imm, ()), tup_setup @ idx_setup @ new_setup)
    | ELetRec (bindings, body, _) ->
        let names, new_bindings_setup = List.split (List.map process_bind_let_rec bindings) in
        let new_binds, new_setup = List.split new_bindings_setup in
        let body_ans, body_setup = helpC body in
        (body_ans, List.concat new_setup @ [BLetRec (List.combine names new_binds)] @ body_setup)
    | ENumber (_, _) | EBool (_, _) | ENil _ | EId (_, _) ->
        let imm, setup = helpI e in
        (CImmExpr imm, setup)
    | EChecks _ | ECheckIR _ | ESingleCheck _ -> ice "ECheck should be desugared by now"
  and helpI (e : tag expr) : unit immexpr * unit anf_bind list =
    match e with
    | ENumber (n, _) -> (ImmNum (n, ()), [])
    | EBool (b, _) -> (ImmBool (b, ()), [])
    | EId (name, _) -> (ImmId (name, ()), [])
    | ENil _ -> (ImmNil (), [])
    | ESeq (e1, e2, _) ->
        let _e1_imm, e1_setup = helpI e1 in
        let e2_imm, e2_setup = helpI e2 in
        (e2_imm, e1_setup @ e2_setup)
    | ETuple (args, tag) ->
        let tmp = sprintf "tup_%d" tag in
        let new_args, new_setup = List.split (List.map helpI args) in
        (ImmId (tmp, ()), List.concat new_setup @ [BLet (tmp, CTuple (new_args, ()))])
    | EGetItem (tup, idx, tag) ->
        let tmp = sprintf "get_%d" tag in
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        (ImmId (tmp, ()), tup_setup @ idx_setup @ [BLet (tmp, CGetItem (tup_imm, idx_imm, ()))])
    | ESetItem (tup, idx, newval, tag) ->
        let tmp = sprintf "set_%d" tag in
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        let new_imm, new_setup = helpI newval in
        ( ImmId (tmp, ()),
          tup_setup @ idx_setup @ new_setup @ [BLet (tmp, CSetItem (tup_imm, idx_imm, new_imm, ()))]
        )
    | EPrim1 (op, arg, tag) ->
        let tmp = sprintf "unary_%d" tag in
        let arg_imm, arg_setup = helpI arg in
        (ImmId (tmp, ()), arg_setup @ [BLet (tmp, CPrim1 (op, arg_imm, ()))])
    | EPrim2 (op, left, right, tag) ->
        let tmp = sprintf "binop_%d" tag in
        let left_imm, left_setup = helpI left in
        let right_imm, right_setup = helpI right in
        ( ImmId (tmp, ()),
          left_setup @ right_setup @ [BLet (tmp, CPrim2 (op, left_imm, right_imm, ()))] )
    | EIf (cond, _then, _else, tag) ->
        let tmp = sprintf "if_%d" tag in
        let cond_imm, cond_setup = helpI cond in
        (ImmId (tmp, ()), cond_setup @ [BLet (tmp, CIf (cond_imm, helpA _then, helpA _else, ()))])
    | EApp (func, args, native, tag) ->
        let tmp = sprintf "app_%d" tag in
        let new_func, func_setup = helpI func in
        let new_args, new_setup = List.split (List.map helpI args) in
        ( ImmId (tmp, ()),
          func_setup @ List.concat new_setup @ [BLet (tmp, CApp (new_func, new_args, native, ()))]
        )
    | ELet ([(BName (name, _, _), value, _)], body, _) ->
        let value_ans, value_ctx = helpI value in
        let body_ans, body_ctx = helpI body in
        (body_ans, value_ctx @ [BLet (name, CImmExpr value_ans)] @ body_ctx)
    | ELet _ -> ice "anf expects fully desugared lets"
    | ELetRec (bindings, body, tag) ->
        let tmp = sprintf "lam_%d" tag in
        let names, new_binds_setup = List.split (List.map process_bind_let_rec bindings) in
        let new_binds, new_setup = List.split new_binds_setup in
        let body_ans, body_setup = helpC body in
        ( ImmId (tmp, ()),
          List.concat new_setup
          @ [BLetRec (List.combine names new_binds)]
          @ body_setup
          @ [BLet (tmp, body_ans)] )
    | ELambda (args, body, tag) ->
        let tmp = sprintf "lam_%d" tag in
        (ImmId (tmp, ()), [BLet (tmp, CLambda (List.map process_bind_lambda args, helpA body, ()))])
    | EChecks _ | ECheckIR _ | ESingleCheck _ -> ice "ECheck should be desugared by now"
  and helpA e : unit aexpr =
    let ans, ans_setup = helpC e in
    List.fold_right
      (fun bind body ->
        match bind with
        | BLet (name, exp) -> ALet (name, exp, body, ())
        | BLetRec names -> ALetRec (names, body, ()) )
      ans_setup (ACExpr ans)
  in
  helpA p
;;
