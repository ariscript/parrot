open Printf
open Exprs
open Pretty

(** parse-error message *)
exception ParseError of string

(** name, where used *)
exception UnboundId of string * sourcespan

(** name, where used, where defined *)
exception ShadowId of string * sourcespan * sourcespan option

(** name, where used, where defined *)
exception DuplicateArg of string * sourcespan * sourcespan

(** value, where used *)
exception Overflow of int64 * sourcespan

(** intended arity, actual arity, where called *)
exception Arity of int * int * sourcespan

(** we will do this eventually *)
exception NotYetImplemented of string

(** we make no promise of doing this *)
exception Unsupported of string * sourcespan

(** Major failure: message to show *)
exception InternalCompilerError of string

(** name binding, where defined *)
exception LetRecNonFunction of sourcespan bind * sourcespan

(** where defined *)
exception LetRecInvalidBind of sourcespan bind

let known_compiletime_exn exn =
  match exn with
  | ParseError _
   |NotYetImplemented _
   |InternalCompilerError _
   |UnboundId _
   |ShadowId _
   |DuplicateArg _
   |Overflow _
   |LetRecNonFunction _
   |Unsupported _
   |Arity _ -> true
  | _ -> false
;;

let print_error exn =
  match exn with
  | ParseError msg -> msg
  | NotYetImplemented msg -> "Not yet implemented: " ^ msg
  | Unsupported (msg, loc) -> sprintf "Unsupported: %s at <%s>" msg (string_of_sourcespan loc)
  | InternalCompilerError msg -> "Internal Compiler Error: " ^ msg
  | UnboundId (x, loc) ->
      sprintf "The identifier %s, used at <%s>, is not in scope" x (string_of_sourcespan loc)
  | ShadowId (x, loc, existing) ->
      sprintf "The identifier %s, defined at <%s>, shadows one defined at <%s>" x
        (string_of_sourcespan loc)
        ( match existing with
        | None -> "builtin"
        | Some place -> string_of_sourcespan place )
  | DuplicateArg (x, loc, existing) ->
      sprintf "The argument %s, redefined at <%s>, duplicates one at <%s>" x
        (string_of_sourcespan loc) (string_of_sourcespan existing)
  | Overflow (num, loc) ->
      sprintf "The number literal %Ld, used at <%s>, is not supported in this language" num
        (string_of_sourcespan loc)
  | Arity (expected, actual, loc) ->
      sprintf "The function called at <%s> expected an arity of %d, but received %d arguments"
        (string_of_sourcespan loc) expected actual
  | LetRecNonFunction (bind, loc) ->
      sprintf "Binding error at %s: Let-rec expected a name binding to a lambda; got %s"
        (string_of_sourcespan loc) (string_of_bind bind)
  | _ -> sprintf "%s" (Printexc.to_string exn)
;;

(* Stringifies a list of compilation errors *)
let print_errors (exns : exn list) : string list = List.map print_error exns
