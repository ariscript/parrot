open Errors
open Printf
open Assembly
open Exprs

let ( << ) f g x = f (g x)

let ( >> ) f g x = g (f x)

module StringSet = struct
  include Set.Make (String)

  let union3 s1 s2 s3 = union s1 (union s2 s3)

  let union4 s1 s2 s3 s4 = union (union s1 s2) (union s3 s4)

  let union_all lst = List.fold_left (fun acc x -> union acc x) empty lst
end

type 'a name_envt = (string * 'a) list

type 'a tag_envt = (tag * 'a) list

(** Similar to [failwith], raise the message as an [InternalCompilerError]. *)
let ice (msg : string) = raise (InternalCompilerError msg)

(** Similar to [failwith], raise the message as a [NotYetImplemented]. *)
let todo (msg : string) = raise (NotYetImplemented msg)

(* seeing this magic string can be confusing as it happens in several contexts *)

(** Generate a symbol from the given (readable) name and tag. *)
let tagged_symbol = sprintf "%s#%d"

(** Lookup an identifer [x] in [env], raising an ICE if it is not present. *)
let lookup (env : 'a name_envt tag_envt) (t : tag) (x : string) : 'a =
  match List.assoc_opt t env with
  | Some x1 -> (
    match List.assoc_opt x x1 with
    | Some x -> x
    | None -> ice (sprintf "name `(tag %d)->%s` not found in environment" t x) )
  | None ->
      ice
        (sprintf "what the simga: tag %d not found in environment (was looking for %s)\n%s" t x
           ( "Envs:\n"
           ^ ExtString.String.join "\n"
               (List.map
                  (fun (tag, env) ->
                    sprintf "tag %d" tag
                    ^ ":\n\t"
                    ^ ExtString.String.join "\n\t"
                        (List.map (fun (name, arg) -> name ^ "=>" ^ string_of_arg arg) env) )
                  env ) ) )
;;

(** Group the given list of pairs into a list associating keys with all values they appear with. *)
let group_by l =
  List.fold_left
    (fun seen (k, v) ->
      ( match List.assoc_opt k seen with
      | None -> (k, [v])
      | Some l -> (k, v :: l) )
      :: List.remove_assoc k seen )
    [] l
;;

let rec ungroup l =
  match l with
  | [] -> []
  | (fn, (name, loc)) :: ls ->
      ( fn,
        (name, loc)
        :: List.filter_map
             (fun (fn2, (name2, loc2)) -> if fn = fn2 then Some (name2, loc2) else None)
             ls )
      :: ungroup (List.filter (fun (fn2, _) -> fn <> fn2) ls)
;;

(** Create a list of pairs of items from the original list and their indices. *)
let enumerate l = List.combine l (List.init (List.length l) Fun.id)

(** Take up to the first [n] items in list [l], returning the whole list if
    [l] has fewer than [n] elements *)
let rec take_up_to n l =
  match l with
  | [] -> []
  | x :: xs ->
      if n = 0 then
        []
      else
        x :: take_up_to (n - 1) xs
;;

(** Drop up to the first [n] items in list [l], returning the empty list if
    [l] has fewer than [n] elements. *)
let rec drop_up_to n l =
  match l with
  | [] -> []
  | _ :: xs ->
      if n = 0 then
        l
      else
        drop_up_to (n - 1) xs
;;

let fst3 (x, _, _) = x

let push_big ?(move = true) const message =
  (if move then [IMov (Reg R11, HexConst const)] else [])
  @ [IInstrComment (IPush (Reg R11), message)]
;;

let rec split3 = function
  | [] -> ([], [], [])
  | (x, y, z) :: l ->
      let rx, ry, rz = split3 l in
      (x :: rx, y :: ry, z :: rz)
;;

let[@warning "-27"] aexpr_template (e : 'a aexpr) : 'b =
  let rec helpA = function
    | ALet (name, value, body, t) -> todo ""
    | ALetRec (bindings, body, t) -> todo ""
    | ACExpr c -> helpC c
  and helpC = function
    | CLambda (args, body, t) -> todo ""
    | CImmExpr imm -> helpI imm
    | CPrim1 (op, arg, t) -> todo ""
    | CPrim2 (op, l, r, t) -> todo ""
    | CIf (c, thn, els, t) -> todo ""
    | CTuple (items, t) -> todo ""
    | CGetItem (tup, idx, t) -> todo ""
    | CSetItem (tup, idx, v, t) -> todo ""
    | CSeq (fst, snd, t) -> todo ""
    | CApp (fn, args, ct, t) -> todo ""
  and helpI = function
    | ImmNil t -> todo ""
    | ImmNum (n, t) -> todo ""
    | ImmBool (b, t) -> todo ""
    | ImmId (id, t) -> todo ""
  in
  helpA e
;;
