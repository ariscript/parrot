open Exprs
open Utils
open Printf

(** Desugar [let] and [lambda] expressions such that each only contains one name binding. *)
let desugar_bindings (e : tag expr) : unit expr =
  let rec help_tup_bind name = function
    | ELet ((BTuple (items, _), _, bt) :: binds, body, t) ->
        let layer_removed =
          List.fold_right
            (fun (bind, idx) acc ->
              let bind' =
                [(bind, EGetItem (EId (name, bt), ENumber (Int64.of_int idx, bt), bt), bt)]
              in
              ELet (bind', acc, t) )
            (enumerate items)
            (ELet (binds, body, t))
        in
        help layer_removed
    | _ -> ice "help_tup_bind expects only ELet with tuple bindings"
  and help = function
    | ELet ([], body, _) -> help body
    | ELet ((b, value, bt) :: binds, body, t) as e -> (
        let rest = help (ELet (binds, body, t)) in
        match b with
        | BBlank _ -> ESeq (help value, rest, bt)
        | BName (name, s, name_tag) -> ELet ([(BName (name, s, name_tag), help value, bt)], rest, t)
        | BTuple (binds, _) ->
            let val_name = tagged_symbol "destruct" (get_tag_E value) in
            ELet
              ( [(BName (val_name, false, t), help value, bt)],
                ESeq
                  ( EPrim2
                      ( CheckSize,
                        EId (val_name, t),
                        ENumber (Int64.of_int (List.length binds), t),
                        t ),
                    help_tup_bind val_name e,
                    t ),
                t ) )
    | ELambda (args, body, lam_tag) ->
        let base_binds =
          args
          |> List.mapi (fun i b ->
                 match b with
                 | BName _ -> (b, false)
                 | _ -> (BName (sprintf "fun#%d_arg%d" lam_tag i, false, lam_tag), true) )
        in
        let paired = List.combine args base_binds in
        let new_body =
          List.fold_right
            (fun (source, (dest, generated)) expr ->
              if generated then
                ELet
                  ( [(source, EId (dest |> all_names |> List.hd |> fst3, lam_tag), lam_tag)],
                    expr,
                    lam_tag )
              else
                expr )
            paired body
        in
        ELambda (base_binds |> List.map fst, help new_body, lam_tag)
        (* we know that all [ELetRec]s contain exactly one name binding per [is_well_formed] *)
    | ELetRec (bindings, body, t) ->
        let new_bindings = List.map (fun (bind, expr, t1) -> (bind, help expr, t1)) bindings in
        ELetRec (new_bindings, help body, t)
    | EPrim2 (op, l, r, t) -> EPrim2 (op, help l, help r, t)
    | EPrim1 (op, arg, t) -> EPrim1 (op, help arg, t)
    | EIf (c, t, e, tag) -> EIf (help c, help t, help e, tag)
    | EApp (fn, args, ct, t) -> EApp (help fn, List.map help args, ct, t)
    | ESeq (f, s, t) -> ESeq (help f, help s, t)
    | ETuple (items, t) -> ETuple (List.map help items, t)
    | EGetItem (tup, idx, t) -> EGetItem (help tup, help idx, t)
    | ESetItem (tup, idx, v, t) -> ESetItem (help tup, help idx, help v, t)
    | ENumber (n, t) -> ENumber (n, t)
    | EBool (b, t) -> EBool (b, t)
    | EId (x, t) -> EId (x, t)
    | ENil t -> ENil t
    | EChecks (checks, t) ->
        EChecks
          ( List.map
              (function
                | CSCheck (v, l, r, b, t) -> CSCheck (v, help l, help r, Option.map help b, t)
                | CSVal _ -> ice "CSVal should be desugared away by now" )
              checks,
            t )
    | ECheckIR _ | ESingleCheck _ -> ice "DesugarCheck should be run before desugar"
  in
  e |> help |> untagE
;;

(** Desugars short-circuiting logic operators (and, or) into their equivalent if expressions. *)
let rec desugar_logic =
  let desugar_binding (name, value, _) = (name, desugar_logic value, ()) in
  function
  | EPrim2 (And, l, r, _) ->
      EIf
        ( EPrim1 (Not, EPrim1 (Not, desugar_logic l, ()), ()),
          EPrim1 (Not, EPrim1 (Not, desugar_logic r, ()), ()),
          EBool (false, ()),
          () )
  | EPrim2 (Or, l, r, _) ->
      EIf
        ( EPrim1 (Not, EPrim1 (Not, desugar_logic l, ()), ()),
          EBool (true, ()),
          EPrim1 (Not, EPrim1 (Not, desugar_logic r, ()), ()),
          () )
  | EPrim2 (op, l, r, _) -> EPrim2 (op, desugar_logic l, desugar_logic r, ())
  | EPrim1 (op, arg, _) -> EPrim1 (op, desugar_logic arg, ())
  | EIf (c, t, e, _) -> EIf (desugar_logic c, desugar_logic t, desugar_logic e, ())
  | ELet (binds, body, _) -> ELet (List.map desugar_binding binds, desugar_logic body, ())
  | EApp (fn, args, ct, _) -> EApp (fn, List.map desugar_logic args, ct, ())
  | ESeq (f, s, _) -> ESeq (desugar_logic f, desugar_logic s, ())
  | ETuple (items, _) -> ETuple (List.map desugar_logic items, ())
  | EGetItem (tup, idx, _) -> EGetItem (desugar_logic tup, desugar_logic idx, ())
  | ESetItem (tup, idx, v, _) -> ESetItem (desugar_logic tup, desugar_logic idx, desugar_logic v, ())
  | ELetRec (binds, body, _) -> ELetRec (List.map desugar_binding binds, desugar_logic body, ())
  | ELambda (args, body, _) -> ELambda (args, desugar_logic body, ())
  | ENumber (n, _) -> ENumber (n, ())
  | EBool (b, _) -> EBool (b, ())
  | EId (x, _) -> EId (x, ())
  | ENil _ -> ENil ()
  | EChecks (checks, _) ->
      let checks' =
        List.map
          (function
            | CSCheck (var, l, r, b, _) ->
                CSCheck (var, desugar_logic l, desugar_logic r, Option.map desugar_logic b, ())
            | CSVal _ -> ice "CSVal should be desugared away by now" )
          checks
      in
      EChecks (checks', ())
  | ECheckIR _ | ESingleCheck _ -> ice "DesugarCheck should be run before desugar"
;;

let desugar e = e |> tag |> desugar_bindings |> desugar_logic |> tag
