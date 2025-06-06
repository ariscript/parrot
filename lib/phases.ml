open Printf
open Exprs
open Errors
open Pretty
open Assembly
open Utils

(* There are lots of ways to work with pipelines of functions that "can fail
   at any point". They all have various drawbacks, though.  See
   http://keleshev.com/composable-error-handling-in-ocaml for a decent writeup
   of some of the techniques.  Since we haven't introduced all of the OCaml
   concepts mentioned there, this file uses a variation on the ideas shown in
   that blog post. *)

(* Describes individual phases of compilation.
   Feel free to add additional constructors here, and
   add a "helper" function just afterward. *)
type phase =
  | Source of string
  | Parsed of sourcespan program
  | Exprified of sourcespan expr
  | Devalued of sourcespan expr
  | WellFormed of sourcespan expr
  | CheckDesugared of sourcespan expr
  | Injected of tag expr
  | Desugared of tag expr
  | Renamed of tag expr
  | Tagged of tag expr
  | ANFed of tag aexpr
  | Closed of tag aexpr
  | Located of tag aexpr * arg name_envt tag_envt
  | Result of string
  | FreeCached of (StringSet.t * tag) aexpr
  | DeadVarsEliminated of (StringSet.t * tag) aexpr

(* These functions simply apply a phase constructor, because OCaml
   doesn't allow you to pass data-constructors as first-class values *)
let source s = Source s

let parsed p = Parsed p

let exprified p = Exprified p

let devalued p = Devalued p

let well_formed e = WellFormed e

let desugar_check e = CheckDesugared e

let renamed e = Renamed e

let desugared e = Desugared e

let tagged e = Tagged e

let anfed e = ANFed e

let injected e = Injected e

let closed e = Closed e

let locate_bindings (expr, env) = Located (expr, env)

let free_cached x = FreeCached x

let dead_vars_eliminated x = DeadVarsEliminated x

let result s = Result s

(* When a stage of the compiler fails, return all the errors that occured,
   along with whatever phases of the compiler successfully completed *)
type failure = exn list * phase list

(* An individual function might fail sometimes, and either return a value
   or a bunch of errors *)
type 'a fallible = ('a, exn list) result

(* An overall pipeline returns either a final result (of type 'a) and
   a list of prior phases, or it returns a failure as above *)
type 'a pipeline = ('a * phase list, failure) result

(* Adds another phase to the growing pipeline, using a function that might fail.
   If the function returns an Error full of exns, then the pipeline dies right there.
   If the function *throws* an exception, the pipeline dies right there.
   If the function succeeds, then the pipeline grows (using log to add the result
   onto the pipeline).
   NOTE: Executing add_err_phase will never throw any exceptions.
*)
let add_err_phase (log : 'b -> phase) (next : 'a -> 'b fallible) (cur_pipeline : 'a pipeline) :
    'b pipeline =
  match cur_pipeline with
  | Error (errs, trace) -> Error (errs, trace)
  | Ok (cur_val, trace) -> (
    try
      match next cur_val with
      | Error errs -> Error (errs, trace)
      | Ok new_val -> Ok (new_val, log new_val :: trace)
    with
    | Failure s -> Error ([Failure ("Compile error: " ^ s)], trace)
    | err ->
        if known_compiletime_exn err then
          Error ([Failure (print_error err)], trace)
        else
          Error ([Failure ("Unexpected compile error: " ^ Printexc.to_string err)], trace) )
;;

(* Adds another phase to the growing pipeline, using a function that should never fail.
   If the function *throws* an exception, the pipeline dies right there.
   Otherwise, the pipeline grows (using log to add the result onto the pipeline).
   NOTE: Executing add_phase will never throw any exceptions.
*)
let add_phase (log : 'b -> phase) (next : 'a -> 'b) (cur_pipeline : 'a pipeline) : 'b pipeline =
  match cur_pipeline with
  | Error (errs, trace) -> Error (errs, trace)
  | Ok (cur_val, trace) -> (
    try
      let new_val = next cur_val in
      Ok (new_val, log new_val :: trace)
    with
    | Failure s -> Error ([Failure ("Compile error: " ^ s)], trace)
    | err ->
        if known_compiletime_exn err then
          Error ([Failure (print_error err)], trace)
        else
          Error ([Failure ("Unexpected compile error: " ^ Printexc.to_string err)], trace) )
;;

let no_op_phase (cur_pipeline : 'a pipeline) = cur_pipeline

(* Stringifies a list of phases, for debug printing purposes *)
let print_trace (trace : phase list) : string list =
  let phase_name p =
    match p with
    | Source _ -> "Source"
    | Parsed _ -> "Parsed"
    | Exprified _ -> "Undef'ed"
    | WellFormed _ -> "Well-formed"
    | CheckDesugared _ -> "CheckDesugared"
    | Renamed _ -> "Renamed"
    | Desugared _ -> "Desugared"
    | Tagged _ -> "Tagged"
    | ANFed _ -> "ANF'ed"
    | Injected _ -> "Injected"
    | Closed _ -> "Closed"
    | Located _ -> "Located"
    | Result _ -> "Result"
    | FreeCached _ -> "FreeCached"
    | DeadVarsEliminated _ -> "DeadVarsEliminated"
    | Devalued _ -> "Devalued"
  in
  let string_of_phase p =
    match p with
    | Source s -> s
    | Parsed p -> string_of_program p
    | Exprified e | WellFormed e | CheckDesugared e | Devalued e -> string_of_expr e
    | Desugared e | Injected e -> string_of_expr e
    | Renamed e -> string_of_expr e
    | FreeCached e ->
        string_of_aexpr_with 1000
          (fun tag -> sprintf "@%s" ([%show: string list] (StringSet.to_list (fst tag))))
          e
    | Tagged e -> string_of_expr_with 1000 (fun tag -> sprintf "@%d" tag) e
    | ANFed e | Closed e -> string_of_aexpr_with 1000 (fun tag -> sprintf "@%d" tag) e
    | Located (p, e) ->
        string_of_aexpr_with 1000 (fun tag -> sprintf "@%d" tag) p
        ^ "\nEnvs:\n"
        ^ ExtString.String.join "\n"
            (List.map
               (fun (tag, env) ->
                 sprintf "tag %d" tag
                 ^ ":\n\t"
                 ^ ExtString.String.join "\n\t"
                     (List.map (fun (name, arg) -> name ^ "=>" ^ string_of_arg arg) env) )
               e )
    | DeadVarsEliminated e -> string_of_aexpr_with 1000 (fun tag -> sprintf "@%d" (snd tag)) e
    | Result s -> s
  in
  List.mapi
    (fun n p -> sprintf "Phase %d (%s):\n%s" n (phase_name p) (string_of_phase p))
    (List.rev trace)
;;
