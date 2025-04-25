open Exprs
open Utils
open Common

(** Desugars a program into an expression by converting function [def]initions
    into [let rec]s bound to [lambda]s.
    - This preserves the original semantics of disallowing duplicate top-level
      [def]initions by not marking the resuling  let bindings as [shadow]ed.
    - Care is taken to preserve relevant sourcespan information is preserved in
      the generated binding where possible.
      - We do fill in garbage position information in some cases, but these will 
        not be reported in any errors, so this is a non-issue. *)
type 'a exprify_type =
  | Let of 'a binding list
  | Check of 'a check_statement list * 'a

(* [g] must be either only a singleton [DCheck] or a (non-empty) list of [DFun] *)
let single_group (g : sourcespan decl list) : sourcespan exprify_type =
  List.fold_left
    (fun [@warning "-8"] (Let acc) -> function[@warning "+8"]
      | DFun (name, args, expr, span) ->
          Let ((BName (name, false, span), ELambda (args, expr, span), span) :: acc)
      | DCheck _ -> ice "DCheck top-level should be singleton" )
    ((function
       | DFun (name, args, expr, span) ->
           Let [(BName (name, false, span), ELambda (args, expr, span), span)]
       | DCheck (checks, span) -> Check (checks, span) )
       (List.hd g) )
    (List.tl g)
;;

let exprify (Program (dgroups, body, _)) : sourcespan expr =
  List.fold_right
    (fun group acc ->
      match single_group group with
      | Let bindings -> ELetRec (bindings, acc, dummy_span)
      | Check (checks, a) -> ESeq (EChecks (checks, a), acc, dummy_span) )
    dgroups body
;;
