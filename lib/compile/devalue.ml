open Exprs
open Common
open Utils

(** Check blocks allow having [CSVal]s which allow binding values to make 
    testing nicer. However, they exist as a [check_variant list] inside of
    [EChecks]. To make our lives easier, we perform the following conversions:
    - [CSVal]s are transformed into [ELet]s, placing the following 
      [check_varients] as the body.
    - [CSCheck]s will be converted to [ESingleCheck]s
    Additionally, check blocks will always return [ENil], but later in the 
    compiler, we should be maintianing the invariant that they are always the
    left side of a sequence. *)

let rec devalue : sourcespan expr -> sourcespan expr = function
  | EChecks (items, _) ->
      ECheckIR
        ( List.fold_right
            (fun x acc ->
              match x with
              | CSVal (b, v, t) -> ELet ([(b, v, t)], acc, dummy_span)
              | CSCheck _ -> ESeq (ESingleCheck x, acc, dummy_span) )
            items (ENil dummy_span),
          dummy_span )
  | ESeq (e1, e2, a) -> ESeq (devalue e1, devalue e2, a)
  | ETuple (exprs, a) -> ETuple (List.map devalue exprs, a)
  | EGetItem (e, idx, a) -> EGetItem (devalue e, devalue idx, a)
  | ESetItem (e, idx, newval, a) -> ESetItem (devalue e, devalue idx, devalue newval, a)
  | EId (x, a) -> EId (x, a)
  | ENumber (n, a) -> ENumber (n, a)
  | EBool (b, a) -> EBool (b, a)
  | ENil a -> ENil a
  | EPrim1 (op, e, a) -> EPrim1 (op, devalue e, a)
  | EPrim2 (op, e1, e2, a) -> EPrim2 (op, devalue e1, devalue e2, a)
  | ELet (binds, body, a) ->
      let binds' = List.map (fun (b, e, t) -> (b, devalue e, t)) binds in
      ELet (binds', devalue body, a)
  | ELetRec (binds, body, a) ->
      let binds' = List.map (fun (b, e, t) -> (b, devalue e, t)) binds in
      ELetRec (binds', devalue body, a)
  | EIf (cond, thn, els, a) -> EIf (devalue cond, devalue thn, devalue els, a)
  | EApp (func, args, native, a) -> EApp (devalue func, List.map devalue args, native, a)
  | ELambda (binds, body, a) -> ELambda (binds, devalue body, a)
  | ECheckIR (_, _) | ESingleCheck _ -> ice "not constructable via surface syntax"
;;
