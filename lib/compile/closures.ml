open Exprs
open Utils

let closure_conversion (e : tag aexpr) : tag aexpr =
  let rec helpA e =
    match e with
    | ACExpr c -> ACExpr (helpC c)
    | ALet (name, value, body, tag) -> ALet (name, helpC value, helpA body, tag)
    | ALetRec (bindings, body, tag) ->
        ALetRec (List.map (fun (name, value) -> (name, helpC value)) bindings, helpA body, tag)
  and helpC e =
    match e with
    | CApp (fn, args, Native, tag) -> CApp (fn, args, Native, tag)
    | CApp (fn, args, ty, tag) -> CApp (fn, fn :: args, ty, tag)
    | CIf (c, t, e, tag) -> CIf (c, helpA t, helpA e, tag)
    | CSeq (f, s, tag) -> CSeq (helpA f, helpA s, tag)
    | CLambda (args, body, tag) -> CLambda (tagged_symbol "closure" tag :: args, helpA body, tag)
    | _ -> e
  in
  helpA e
;;
