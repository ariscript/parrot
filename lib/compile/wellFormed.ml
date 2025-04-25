open Exprs
open Utils
open Common
open Phases
open Errors

let duplicate_errors grouped ctor =
  List.fold_left
    (fun acc (name, occurences) ->
      match occurences with
      | [] -> ice "item must have occurred at least once to be in the list"
      | [_] -> acc
      | f :: other -> acc @ List.map (fun pos -> ctor (name, f, pos)) other )
    [] grouped
;;

(** Ensures that a program is well-formed, meaning that:
  - functions must be bound if used
  - functions must not be duplicated
  - identifiers must be bound if used
  - identifiers must not be duplicated within the same let expression 
  - function arguments must not be duplicated within the same function
  - functions must be called with the same number of arguments as it declares parameters
  - integer literals must fit within the 63-bit representation of snakevals
  
  If the program is not well-formed, returns a list of all errors found in the program. *)
let is_well_formed (e : sourcespan expr) : sourcespan expr fallible =
  let rec help_letrec (binds : sourcespan bind list) (seen : (string * sourcespan list) list) errors
      =
    match binds with
    | [] -> errors
    | BName (name, shadow, pos) :: binds -> (
        if shadow then
          help_letrec binds seen errors
        else
          match List.assoc_opt name seen with
          | None -> help_letrec binds seen errors
          | Some places ->
              help_letrec binds
                ((name, pos :: List.assoc name seen) :: List.remove_assoc name seen)
                (errors @ List.map (fun place -> ShadowId (name, pos, Some place)) places) )
    | bind :: binds -> help_letrec binds seen (errors @ [LetRecInvalidBind bind])
  in
  let rec wf_E (vars : (string * sourcespan option) list) errors e =
    match e with
    | EBool (_, _) | ENil _ -> errors
    | ENumber (n, p) ->
        if n > Int64.div Int64.max_int 2L || n < Int64.div Int64.min_int 2L then
          errors @ [Overflow (n, p)]
        else
          errors
    | EId (x, pos) ->
        if List.mem_assoc x vars then
          errors
        else
          errors @ [UnboundId (x, pos)]
    | EIf (c, t, e, _) ->
        let errors = wf_E vars errors c in
        let errors = wf_E vars errors t in
        wf_E vars errors e
    | EPrim1 (_, arg, _) -> wf_E vars errors arg
    | EPrim2 (_, l, r, _) ->
        let errors = wf_E vars errors l in
        wf_E vars errors r
    | ELet ([], body, _) -> wf_E vars errors body
    | ELet ((bind, value, _) :: bindings, body, p) ->
        let new_binds = all_names bind in
        let new_names = new_binds |> List.map (fun (name, _, pos) -> (name, Some pos)) in
        let errors = wf_E vars errors value in
        let errors =
          List.fold_left
            (fun errors (name, shadow, pos) ->
              let grouped = group_by vars in
              if shadow then
                errors
              else
                let prev_defs = List.assoc_opt name grouped in
                match prev_defs with
                | None -> errors
                | Some places -> errors @ List.map (fun p -> ShadowId (name, pos, p)) places )
            errors new_binds
        in
        wf_E (new_names @ vars) errors (ELet (bindings, body, p))
    | ELetRec (bindings, body, _) ->
        let new_binds = List.concat_map all_names (List.map fst3 bindings) in
        let new_names = new_binds |> List.map (fun (name, _, pos) -> (name, Some pos)) in
        let name_errs =
          List.fold_left
            (fun (errs, seen) (name, shadow, src) ->
              ( ( match List.assoc_opt name seen with
                | Some x when not shadow -> ShadowId (name, src, Some x) :: errs
                | _ -> errs ),
                (name, src) :: seen ) )
            ([], []) new_binds
          |> fst
        in
        let errors =
          List.fold_left
            (fun errors (bind, value, _) ->
              let grouped = group_by vars in
              let invalid_binds =
                match bind with
                | BName (_, true, _) -> []
                | BName (name, false, pos) ->
                    List.assoc_opt name grouped
                    |> Option.map (List.map (fun p -> ShadowId (name, pos, p)))
                    |> Option.default []
                | _ -> [LetRecInvalidBind bind]
              in
              let invalid_val =
                match value with
                | ELambda _ -> []
                | _ -> [LetRecNonFunction (bind, get_tag_E value)]
              in
              errors @ invalid_binds @ invalid_val )
            (name_errs @ errors) bindings
        in
        let errors = errors @ help_letrec (List.map fst3 bindings) [] [] in
        let errors =
          List.fold_left
            (fun errors (_, value, _) -> wf_E (new_names @ vars) errors value)
            errors bindings
        in
        wf_E (new_names @ vars) errors body
    | ELambda (args, body, _) ->
        let binds = List.concat_map all_names args in
        let new_names = List.map (fun (name, _, pos) -> (name, Some pos)) binds in
        let grouped = group_by new_names in
        let errors =
          errors
          @ duplicate_errors grouped (fun (name, f, r) ->
                (* these options will definitely have values in them, args will not
                        be from builtin. *)
                DuplicateArg (name, Option.get f, Option.get r) )
        in
        wf_E (new_names @ vars) errors body
    | EApp (fn, args, _, _) ->
        let errors = wf_E vars errors fn in
        List.fold_left (wf_E vars) errors args
    | ESeq (first, second, _) ->
        let errors = wf_E vars errors first in
        wf_E vars errors second
    | ETuple (items, _) -> List.fold_left (fun errors item -> wf_E vars errors item) errors items
    | EGetItem (tup, idx, _) ->
        let errors = wf_E vars errors tup in
        wf_E vars errors idx
    | ESetItem (tup, idx, v, _) ->
        let errors = wf_E vars errors tup in
        let errors = wf_E vars errors idx in
        wf_E vars errors v
    | EChecks (checks, _) ->
        List.fold_left
          (fun errors -> function
            | CSCheck (_, l, r, b, _) ->
                let errors = wf_E vars errors l in
                let errors = wf_E vars errors r in
                Option.map_default (wf_E vars errors) errors b
            | CSVal _ -> ice "check-examples should have turned into let by now" )
          [] checks
    | ECheckIR (e, _) -> wf_E vars errors e
    (* this is literally what a single check is *)
    | ESingleCheck cs -> wf_E vars errors (EChecks ([cs], dummy_span))
  in
  let builtins = List.map (fun (name, (_, _, src, _)) -> (name, src)) builtins in
  match wf_E builtins [] e with
  | [] -> Ok e
  | l -> Error l
;;
