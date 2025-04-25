open Printf

let show_debug_print = ref false

let debug_printf fmt =
  if !show_debug_print then
    printf fmt
  else
    ifprintf stdout fmt
;;

type tag = int [@@deriving show, eq]

type sourcespan = Lexing.position * Lexing.position

(** A primative operation with one argument *)
type prim1 =
  | Add1
  | Sub1
  | Print
  | IsBool
  | IsNum
  | IsTuple
  | Not
  | PrintStack
[@@deriving show, eq]

type prim2 =
  | Plus
  | Minus
  | Times
  | And
  | Or
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Eq
  | CheckSize
[@@deriving show, eq]

(** The _notion of equality_ used for a check. *)
type 'a equality_notion =
  | NEqual
  | NEq
  | NPred of 'a
[@@deriving show, eq]

and 'a check_variant =
  | Negate of 'a check_variant
  | Is of 'a equality_notion
  | Satisfies
[@@deriving show, eq]

type 'a bind =
  | BBlank of 'a
  | BName of string * bool * 'a
  | BTuple of 'a bind list * 'a
[@@deriving show, eq]

and 'a binding = 'a bind * 'a expr * 'a [@@deriving show, eq]

and call_type =
  | Native
  | Snake
  | Prim
  | Check
  | Unknown
[@@deriving show, eq]

and 'a expr =
  | ESeq of 'a expr * 'a expr * 'a
  | ETuple of 'a expr list * 'a
  | EGetItem of 'a expr * 'a expr * 'a
  | ESetItem of 'a expr * 'a expr * 'a expr * 'a
  | ELet of 'a binding list * 'a expr * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | EIf of 'a expr * 'a expr * 'a expr * 'a
  | ENumber of int64 * 'a
  | EBool of bool * 'a
  | ENil of 'a
  | EId of string * 'a
  | EApp of 'a expr * 'a expr list * call_type * 'a
  | ELambda of 'a bind list * 'a expr * 'a
  | ELetRec of 'a binding list * 'a expr * 'a
  | ECheckIR of 'a expr * 'a (* We're in desperate need of a better IR *)
  | ESingleCheck of 'a check_statement
  | EChecks of 'a check_statement list * 'a  (** NOT DIRECTLY CONSTRUCTABLE BY SURFACE SYNTAX *)
[@@deriving show, eq, visitors {name= "emap"; variety= "map"}]

and 'a check_statement =
  | CSVal of 'a binding
  | CSCheck of 'a expr check_variant * 'a expr * 'a expr * 'a expr option * 'a
[@@deriving show, eq]

type 'a decl =
  | DFun of string * 'a bind list * 'a expr * 'a
  | DCheck of 'a check_statement list * 'a
[@@deriving show, eq]

type 'a program = Program of 'a decl list list * 'a expr * 'a [@@deriving show, eq]

(** immediate expressions *)
type 'a immexpr =
  | ImmNum of int64 * 'a
  | ImmBool of bool * 'a
  | ImmId of string * 'a
  | ImmNil of 'a
[@@deriving show, eq]

(** compound expressions *)
and 'a cexpr =
  | CIf of 'a immexpr * 'a aexpr * 'a aexpr * 'a
  | CPrim1 of prim1 * 'a immexpr * 'a
  | CPrim2 of prim2 * 'a immexpr * 'a immexpr * 'a
  | CApp of 'a immexpr * 'a immexpr list * call_type * 'a
  | CImmExpr of 'a immexpr (* for when you just need an immediate value *)
  | CTuple of 'a immexpr list * 'a
  | CGetItem of 'a immexpr * 'a immexpr * 'a
  | CSetItem of 'a immexpr * 'a immexpr * 'a immexpr * 'a
  | CLambda of string list * 'a aexpr * 'a
  | CSeq of 'a aexpr * 'a aexpr * 'a
[@@deriving show, eq]

(** anf expressions *)
and 'a aexpr =
  | ALet of string * 'a cexpr * 'a aexpr * 'a
  | ALetRec of (string * 'a cexpr) list * 'a aexpr * 'a
  | ACExpr of 'a cexpr
[@@deriving visitors {name= "aemap"; variety= "map"}]

type alloc_strategy = Naive [@@deriving show, eq]

let map_opt f = function
  | None -> None
  | Some v -> Some (f v)
;;

let rec get_tag_E = function
  | ELet (_, _, t) -> t
  | ELetRec (_, _, t) -> t
  | EPrim1 (_, _, t) -> t
  | EPrim2 (_, _, _, t) -> t
  | EIf (_, _, _, t) -> t
  | ENil t -> t
  | ENumber (_, t) -> t
  | EBool (_, t) -> t
  | EId (_, t) -> t
  | EApp (_, _, _, t) -> t
  | ETuple (_, t) -> t
  | EGetItem (_, _, t) -> t
  | ESetItem (_, _, _, t) -> t
  | ESeq (_, _, t) -> t
  | ELambda (_, _, t) -> t
  | EChecks (_, t) -> t
  | ECheckIR (_, t) -> t
  | ESingleCheck ck -> get_tag_Ck ck

and get_tag_Ck = function
  | CSVal (_, _, t) -> t
  | CSCheck (_, _, _, _, t) -> t
;;

let get_tag_D = function
  | DFun (_, _, _, t) -> t
  | DCheck (_, t) -> t
;;

let get_tag_I = function
  | ImmNil t -> t
  | ImmNum (_, t) -> t
  | ImmBool (_, t) -> t
  | ImmId (_, t) -> t
;;

let get_tag_C = function
  | CLambda (_, _, t) -> t
  | CImmExpr imm -> get_tag_I imm
  | CPrim1 (_, _, t) -> t
  | CPrim2 (_, _, _, t) -> t
  | CIf (_, _, _, t) -> t
  | CTuple (_, t) -> t
  | CGetItem (_, _, t) -> t
  | CSetItem (_, _, _, t) -> t
  | CSeq (_, _, t) -> t
  | CApp (_, _, _, t) -> t
;;

let get_tag_A = function
  | ACExpr c -> get_tag_C c
  | ALet (_, _, _, t) -> t
  | ALetRec (_, _, t) -> t
;;

let rec map_tag_E (f : 'a -> 'b) (e : 'a expr) =
  match e with
  | ESeq (e1, e2, a) -> ESeq (map_tag_E f e1, map_tag_E f e2, f a)
  | ETuple (exprs, a) -> ETuple (List.map (map_tag_E f) exprs, f a)
  | EGetItem (e, idx, a) -> EGetItem (map_tag_E f e, map_tag_E f idx, f a)
  | ESetItem (e, idx, newval, a) ->
      ESetItem (map_tag_E f e, map_tag_E f idx, map_tag_E f newval, f a)
  | EId (x, a) -> EId (x, f a)
  | ENumber (n, a) -> ENumber (n, f a)
  | EBool (b, a) -> EBool (b, f a)
  | ENil a -> ENil (f a)
  | EPrim1 (op, e, a) -> EPrim1 (op, map_tag_E f e, f a)
  | EPrim2 (op, e1, e2, a) -> EPrim2 (op, map_tag_E f e1, map_tag_E f e2, f a)
  | ELet (binds, body, a) ->
      let binds' = List.map (fun (b, e, t) -> (map_tag_B f b, map_tag_E f e, f t)) binds in
      ELet (binds', map_tag_E f body, f a)
  | ELetRec (binds, body, a) ->
      let binds' = List.map (fun (b, e, t) -> (map_tag_B f b, map_tag_E f e, f t)) binds in
      ELetRec (binds', map_tag_E f body, f a)
  | EIf (cond, thn, els, a) -> EIf (map_tag_E f cond, map_tag_E f thn, map_tag_E f els, f a)
  | EApp (func, args, native, a) -> EApp (map_tag_E f func, List.map (map_tag_E f) args, native, f a)
  | ELambda (binds, body, a) -> ELambda (List.map (map_tag_B f) binds, map_tag_E f body, f a)
  | EChecks (checks, a) -> EChecks (List.map (map_tag_Ck f) checks, f a)
  | ECheckIR (e, a) -> ECheckIR (map_tag_E f e, f a)
  | ESingleCheck cs -> ESingleCheck (map_tag_Ck f cs)

and map_tag_B (f : 'a -> 'b) b =
  match b with
  | BBlank tag -> BBlank (f tag)
  | BName (x, allow_shadow, ax) -> BName (x, allow_shadow, f ax)
  | BTuple (binds, t) -> BTuple (List.map (map_tag_B f) binds, f t)

and map_tag_D (f : 'a -> 'b) d =
  match d with
  | DFun (name, args, body, a) -> DFun (name, List.map (map_tag_B f) args, map_tag_E f body, f a)
  | DCheck (checks, a) -> DCheck (List.map (map_tag_Ck f) checks, f a)

and map_tag_Ck (f : 'a -> 'b) cs =
  match cs with
  | CSVal (bind, v, t) -> CSVal (map_tag_B f bind, map_tag_E f v, f t)
  | CSCheck (variant, l, r, b, t) ->
      CSCheck (map_tag_C_V f variant, map_tag_E f l, map_tag_E f r, Option.map (map_tag_E f) b, f t)

and map_tag_C_V (f : 'a -> 'b) = function
  | Negate cv -> Negate (map_tag_C_V f cv)
  | Is en -> Is (map_tag_E_N f en)
  | Satisfies -> Satisfies

and map_tag_E_N (f : 'a -> 'b) = function
  | NEqual -> NEqual
  | NEq -> NEq
  | NPred expr -> NPred (map_tag_E f expr)

and map_tag_P (f : 'a -> 'b) p =
  match p with
  | Program (declgroups, body, a) ->
      Program
        (List.map (fun group -> List.map (map_tag_D f) group) declgroups, map_tag_E f body, f a)
;;

let tag (e : 'a expr) : tag expr =
  let next = ref 0 in
  let tag _ =
    next := !next + 1;
    !next
  in
  map_tag_E tag e
;;

let combine_tags (f1 : 'a -> 'b) (f2 : 'a -> 'c) (p : 'a program) : ('b * 'c) program =
  map_tag_P (fun a -> (f1 a, f2 a)) p
;;

let tag_and_map (f : 'a -> 'b) (p : 'a program) : ('a * 'b) program =
  map_tag_P (fun a -> (a, f a)) p
;;

let prog_and_tag (p : 'a program) : ('a * tag) program =
  let next = ref 0 in
  let tag _ =
    next := !next + 1;
    !next
  in
  tag_and_map tag p
;;

let rec untagP (p : 'a program) : unit program =
  match p with
  | Program (decls, body, _) ->
      Program (List.map (fun group -> List.map untagD group) decls, untagE body, ())

and untagE e =
  match e with
  | ESeq (e1, e2, _) -> ESeq (untagE e1, untagE e2, ())
  | ETuple (exprs, _) -> ETuple (List.map untagE exprs, ())
  | EGetItem (e, idx, _) -> EGetItem (untagE e, untagE idx, ())
  | ESetItem (e, idx, newval, _) -> ESetItem (untagE e, untagE idx, untagE newval, ())
  | EId (x, _) -> EId (x, ())
  | ENumber (n, _) -> ENumber (n, ())
  | EBool (b, _) -> EBool (b, ())
  | ENil _ -> ENil ()
  | EPrim1 (op, e, _) -> EPrim1 (op, untagE e, ())
  | EPrim2 (op, e1, e2, _) -> EPrim2 (op, untagE e1, untagE e2, ())
  | ELet (binds, body, _) ->
      ELet (List.map (fun (b, e, _) -> (untagB b, untagE e, ())) binds, untagE body, ())
  | EIf (cond, thn, els, _) -> EIf (untagE cond, untagE thn, untagE els, ())
  | EApp (func, args, native, _) -> EApp (untagE func, List.map untagE args, native, ())
  | ELetRec (binds, body, _) ->
      ELetRec (List.map (fun (b, e, _) -> (untagB b, untagE e, ())) binds, untagE body, ())
  | ELambda (binds, body, _) -> ELambda (List.map untagB binds, untagE body, ())
  | EChecks (checks, _) -> EChecks (List.map untagCk checks, ())
  | ECheckIR (e, _) -> ECheckIR (untagE e, ())
  | ESingleCheck cs -> ESingleCheck (untagCk cs)

and untagB b =
  match b with
  | BBlank _ -> BBlank ()
  | BName (x, allow_shadow, _) -> BName (x, allow_shadow, ())
  | BTuple (binds, _) -> BTuple (List.map untagB binds, ())

and untagD d =
  match d with
  | DFun (name, args, body, _) -> DFun (name, List.map untagB args, untagE body, ())
  | DCheck (checks, _) -> DCheck (List.map untagCk checks, ())

and untagCk (cs : 'a check_statement) =
  match cs with
  | CSVal (bind, v, _) -> CSVal (untagB bind, untagE v, ())
  | CSCheck (variant, l, r, b, _) ->
      CSCheck (untagC_V variant, untagE l, untagE r, Option.map untagE b, ())

and untagC_V = function
  | Negate cv -> Negate (untagC_V cv)
  | Is en -> Is (untagE_N en)
  | Satisfies -> Satisfies

and untagE_N = function
  | NEqual -> NEqual
  | NEq -> NEq
  | NPred expr -> NPred (untagE expr)
;;

let atag (p : 'a aexpr) : tag aexpr =
  let next = ref 0 in
  let tag () =
    next := !next + 1;
    !next
  in
  let rec helpA (e : 'a aexpr) : tag aexpr =
    match e with
    | ALet (x, c, b, _) -> ALet (x, helpC c, helpA b, tag ())
    | ALetRec (xcs, b, _) -> ALetRec (List.map (fun (x, c) -> (x, helpC c)) xcs, helpA b, tag ())
    | ACExpr c -> ACExpr (helpC c)
  and helpC (c : 'a cexpr) : tag cexpr =
    match c with
    | CPrim1 (op, e, _) -> CPrim1 (op, helpI e, tag ())
    | CPrim2 (op, e1, e2, _) -> CPrim2 (op, helpI e1, helpI e2, tag ())
    | CIf (cond, thn, els, _) -> CIf (helpI cond, helpA thn, helpA els, tag ())
    | CApp (func, args, native, _) -> CApp (helpI func, List.map helpI args, native, tag ())
    | CImmExpr i -> CImmExpr (helpI i)
    | CTuple (es, _) -> CTuple (List.map helpI es, tag ())
    | CGetItem (e, idx, _) -> CGetItem (helpI e, helpI idx, tag ())
    | CSetItem (e, idx, newval, _) -> CSetItem (helpI e, helpI idx, helpI newval, tag ())
    | CLambda (args, body, _) -> CLambda (args, helpA body, tag ())
    | CSeq (e1, e2, _) -> CSeq (helpA e1, helpA e2, tag ())
  and helpI (i : 'a immexpr) : tag immexpr =
    match i with
    | ImmNil _ -> ImmNil (tag ())
    | ImmId (x, _) -> ImmId (x, tag ())
    | ImmNum (n, _) -> ImmNum (n, tag ())
    | ImmBool (b, _) -> ImmBool (b, tag ())
  in
  helpA p
;;

let auntag (e : 'a aexpr) : unit aexpr =
  let rec helpA (e : 'a aexpr) : unit aexpr =
    match e with
    | ALet (x, c, b, _) -> ALet (x, helpC c, helpA b, ())
    | ALetRec (xcs, b, _) -> ALetRec (List.map (fun (x, c) -> (x, helpC c)) xcs, helpA b, ())
    | ACExpr c -> ACExpr (helpC c)
  and helpC (c : 'a cexpr) : unit cexpr =
    match c with
    | CPrim1 (op, e, _) -> CPrim1 (op, helpI e, ())
    | CPrim2 (op, e1, e2, _) -> CPrim2 (op, helpI e1, helpI e2, ())
    | CIf (cond, thn, els, _) -> CIf (helpI cond, helpA thn, helpA els, ())
    | CApp (func, args, native, _) -> CApp (helpI func, List.map helpI args, native, ())
    | CImmExpr i -> CImmExpr (helpI i)
    | CTuple (es, _) -> CTuple (List.map helpI es, ())
    | CGetItem (e, idx, _) -> CGetItem (helpI e, helpI idx, ())
    | CSetItem (e, idx, newval, _) -> CSetItem (helpI e, helpI idx, helpI newval, ())
    | CLambda (args, body, _) -> CLambda (args, helpA body, ())
    | CSeq (e1, e2, _) -> CSeq (helpA e1, helpA e2, ())
  and helpI (i : 'a immexpr) : unit immexpr =
    match i with
    | ImmNil _ -> ImmNil ()
    | ImmId (x, _) -> ImmId (x, ())
    | ImmNum (n, _) -> ImmNum (n, ())
    | ImmBool (b, _) -> ImmBool (b, ())
  in
  helpA e
;;

let rec all_names b =
  match b with
  | BBlank _ -> []
  | BName (name, shadow, pos) -> [(name, shadow, pos)]
  | BTuple (inner, _) -> List.concat_map all_names inner
;;

let rec map_atag_A f = function
  | ALet (name, value, body, t) -> ALet (name, map_atag_C f value, map_atag_A f body, f t)
  | ALetRec (bindings, body, t) ->
      let bindings' = List.map (fun (n, c) -> (n, map_atag_C f c)) bindings in
      ALetRec (bindings', map_atag_A f body, f t)
  | ACExpr c -> ACExpr (map_atag_C f c)

and map_atag_C f = function
  | CLambda (args, body, t) -> CLambda (args, map_atag_A f body, f t)
  | CImmExpr imm -> CImmExpr (map_atag_I f imm)
  | CPrim1 (op, arg, t) -> CPrim1 (op, map_atag_I f arg, f t)
  | CPrim2 (op, l, r, t) -> CPrim2 (op, map_atag_I f l, map_atag_I f r, f t)
  | CIf (c, thn, els, t) -> CIf (map_atag_I f c, map_atag_A f thn, map_atag_A f els, f t)
  | CTuple (items, t) ->
      let items' = List.map (map_atag_I f) items in
      CTuple (items', f t)
  | CGetItem (tup, idx, t) -> CGetItem (map_atag_I f tup, map_atag_I f idx, f t)
  | CSetItem (tup, idx, v, t) -> CSetItem (map_atag_I f tup, map_atag_I f idx, map_atag_I f v, f t)
  | CSeq (fst, snd, t) -> CSeq (map_atag_A f fst, map_atag_A f snd, f t)
  | CApp (fn, args, ct, t) ->
      let args' = List.map (map_atag_I f) args in
      CApp (map_atag_I f fn, args', ct, f t)

and map_atag_I f = function
  | ImmNil t -> ImmNil (f t)
  | ImmNum (n, t) -> ImmNum (n, f t)
  | ImmBool (b, t) -> ImmBool (b, f t)
  | ImmId (id, t) -> ImmId (id, f t)
;;

let map_and_atag f =
  let next = ref 0 in
  let tag () =
    next := !next + 1;
    !next
  in
  map_atag_A (fun x -> (f x, tag ()))
;;

(* class ['self] aaa =
  object (self : 'self)
    inherit [_] emap

    method visit_expr env e =
      match e with
      | ENumber (n, a) ->
          let n' = self#visit env n in
          let a' = self#visit_'a env a in
          ENumber (n', a')
      | _ -> super#visit_expr env e
  end *)

(* let v =
  object (self : 'self)
    inherit [_] map_expr

    method visit_expr env e =
      match e with
      | ENumber (n, a) ->
          let n' = self#visit_int64 env n in
          let a' = self#visit_'a env a in
          ENumber (n', a')
      | _ -> super#visit_expr env e
  end
;;
 *)
