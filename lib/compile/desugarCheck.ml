open Exprs
open Common
open Utils

let ds = dummy_span

let snum (i : int) = ENumber (Int64.of_int i, ds)

let equal_tag = 0b00

let eq_tag = 0b01

let pred_tag = 0b10

let sat_tag = 0b11

let neg_flag = 0b0100

let because_flag = 0b1000

let rec tag_of_check_variant ck =
  match ck with
  | Negate ck -> tag_of_check_variant ck lor neg_flag
  | Is NEqual -> equal_tag
  | Is NEq -> eq_tag
  | Is (NPred _) -> pred_tag
  | Satisfies -> sat_tag
;;

let tuple_of_source_span ((st, ed) : sourcespan) =
  ETuple
    ( [ snum st.pos_lnum;
        snum (st.pos_cnum - st.pos_bol);
        snum ed.pos_lnum;
        snum (ed.pos_cnum - ed.pos_bol) ],
      dummy_span )
;;

let inner_inner (v, l, r, b, _) =
  let tag = tag_of_check_variant v in
  let base map_l map_r map_b =
    ESeq
      ( ( match b with
        | None -> EApp (EId ("test", ds), [snum tag; map_l l r; map_r l r], Native, ds)
        | Some b ->
            let r_mapped = map_r l (EId ("r", ds)) in
            (* We need this to maintain the same evaulation order as pyret *)
            ELet
              ( [ (BName ("l", true, ds), l, ds);
                  (BName ("r", true, ds), r, ds);
                  (BName ("b", true, ds), b, ds) ],
                ESeq
                  ( EApp
                      ( EId ("test", ds),
                        [snum (tag lor because_flag); map_b (EId ("b", ds)) r; r_mapped],
                        Native,
                        ds ),
                    EApp
                      (EId ("test", ds), [snum tag; map_l (EId ("l", ds)) r; r_mapped], Native, ds),
                    ds ),
                ds ) ),
        ENil ds,
        ds )
  in
  let rec helper v =
    match v with
    | Negate v -> helper v
    (* these are special because we need to use a different call type to ensure nicer errors and stuff *)
    | Is (NPred p) ->
        ESeq
          ( ( match b with
            | None ->
                ELet
                  ( [(BName ("p", true, ds), p, ds)],
                    EApp
                      ( EId ("test", ds),
                        [snum tag; EApp (EId ("p", ds), [l; r], Snake, ds); EId ("p", ds)],
                        Native,
                        ds ),
                    ds )
            | Some b ->
                ELet
                  ( [ (BName ("l", true, ds), l, ds);
                      (BName ("p", true, ds), p, ds);
                      (BName ("r", true, ds), r, ds);
                      (BName ("b", true, ds), b, ds) ],
                    ESeq
                      ( EApp
                          ( EId ("test", ds),
                            [ snum (tag lor because_flag);
                              EApp (EId ("p", ds), [EId ("b", ds); EId ("r", ds)], Snake, ds);
                              EId ("p", ds) ],
                            Native,
                            ds ),
                        EApp
                          ( EId ("test", ds),
                            [ snum tag;
                              EApp (EId ("p", ds), [EId ("l", ds); EId ("r", ds)], Snake, ds);
                              EId ("p", ds) ],
                            Native,
                            ds ),
                        ds ),
                    ds ) ),
            ENil ds,
            ds )
    | Satisfies ->
        let pred_applier l r = EApp (r, [l], Snake, ds) in
        base pred_applier (fun _ r -> r) pred_applier
    | Is _ -> base Fun.const (fun _ r -> r) Fun.const
  in
  helper v
;;

(** Desugars check blocks into more primative operations. 
    - [ECheckIR] is converted into an immediatly called lambda of call type 
      [Check] so we can customize control flow on errors.


  {b NOTE: This must occur before any tagging takes place, since we require 
  sourcespan information {i at runtime}.} *)
let rec desugar = function
  | ESeq (e1, e2, sp) -> ESeq (desugar e1, desugar e2, sp)
  | ETuple (es, sp) -> ETuple (List.map desugar es, sp)
  | EGetItem (e, idx, sp) -> EGetItem (desugar e, desugar idx, sp)
  | ESetItem (e, idx, newval, sp) -> ESetItem (desugar e, desugar idx, desugar newval, sp)
  | EPrim1 (op, arg, sp) -> EPrim1 (op, desugar arg, sp)
  | EPrim2 (op, left, right, sp) -> EPrim2 (op, desugar left, desugar right, sp)
  | EIf (c, t, f, sp) -> EIf (desugar c, desugar t, desugar f, sp)
  | (EId _ | ENumber _ | EBool _ | ENil _) as e -> e
  | EApp (func, args, ct, sp) -> EApp (desugar func, List.map desugar args, ct, sp)
  | ELet (bindings, body, sp) ->
      let bindings' = List.map (fun (b, e, tag') -> (b, desugar e, tag')) bindings in
      ELet (bindings', desugar body, sp)
  | ELetRec (bindings, body, sp) ->
      let bindings' = List.map (fun (b, e, tag') -> (b, desugar e, tag')) bindings in
      let body' = desugar body in
      ELetRec (bindings', body', sp)
  | ELambda (binds, body, sp) ->
      let body' = desugar body in
      ELambda (binds, body', sp)
  | EChecks (_, _) | ESingleCheck (CSVal _) -> ice "should have been removed in the de-value phase"
  | ECheckIR (expr, sp) -> EApp (ELambda ([], desugar expr, sp), [], Check, dummy_span)
  | ESingleCheck (CSCheck (v, l, r, b, sp)) ->
      ELet
        ( [(BName ("span", true, ds), tuple_of_source_span sp, ds)],
          ESeq
            ( EApp (EId ("init_test", ds), [EId ("span", ds)], Native, ds),
              inner_inner (v, l, r, b, sp),
              ds ),
          ds )
;;
