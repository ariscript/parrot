open Exprs
open OUnit2
open Pretty
open Runner
open Utils

let t ?(dont_elim_dead = false) name program input expected =
  name >:: test_run ~args:[] ~std_input:input ~dont_elim_dead Naive program name expected
;;

let ta name program input expected =
  name >:: test_run_anf ~args:[] ~std_input:input program name expected
;;

let tgc ?(dont_elim_dead = false) name heap_size program input expected =
  name
  >:: test_run
        ~args:[string_of_int heap_size]
        ~std_input:input ~dont_elim_dead Naive program name expected
;;

let tvg ?(dont_elim_dead = false) name program input expected =
  name >:: test_run_valgrind ~args:[] ~std_input:input ~dont_elim_dead Naive program name expected
;;

let tvgc ?(dont_elim_dead = false) name heap_size program input expected =
  name
  >:: test_run_valgrind
        ~args:[string_of_int heap_size]
        ~std_input:input ~dont_elim_dead Naive program name expected
;;

let terr ?(dont_elim_dead = false) name program input expected =
  name >:: test_err ~args:[] ~std_input:input ~dont_elim_dead Naive program name expected
;;

let tgcerr ?(dont_elim_dead = false) name heap_size program input expected =
  name
  >:: test_err
        ~args:[string_of_int heap_size]
        ~std_input:input ~dont_elim_dead Naive program name expected
;;

let tanf name expr expected =
  name >:: fun _ -> assert_equal expected (Compile.T.ANF.anf (tag expr)) ~printer:string_of_aexpr
;;

let tparse name program expected =
  name
  >:: fun _ ->
  assert_equal (untagP expected) (untagP (parse_string name program)) ~printer:string_of_program
;;

let parse_to_anf ?(inject = true) prog =
  Compile.T.(
    parse_string "skibidi" prog
    |> Exprify.exprify
    |> Devalue.devalue
    (* well-formed skipped *)
    |> DesugarCheck.desugar
    |> tag
    |> (if inject then Builtins.inject_builtins else Fun.id)
    |> Desugar.desugar
    |> Rename.rename_and_tag
    |> ANF.anf
    |> atag )
;;

let teq name actual expected = name >:: fun _ -> assert_equal expected actual ~printer:(fun s -> s)

let t_gen
    ?(cmp : 'a -> 'a -> bool = ( = ))
    (* HACK: fun x -> x infers as 'a rather than 'b *)
    ?(map_expected : 'b -> 'a = fun x -> Obj.magic x)
    (printer : 'a -> string)
    (name : string)
    (value : 'a)
    (expected : 'b) =
  name >:: fun _ -> assert_equal (map_expected expected) value ~printer ~cmp
;;

let lst_printer printer l = "[" ^ String.concat "; " (List.map printer l) ^ "]"

let quote_escape s = "\"" ^ String.escaped s ^ "\""

let t_sl : string -> string list -> string list -> test = t_gen (lst_printer quote_escape)

let t_ssl =
  t_gen
    (StringSet.to_list >> lst_printer quote_escape)
    ~map_expected:StringSet.of_list ~cmp:StringSet.equal
;;

let t_aexpr ?(cmp = ( = )) aprint = t_gen (string_of_aexpr_with Int.max_int aprint) ~cmp

let t_fv name src expected =
  t_aexpr
    (fun s -> StringSet.to_list s |> lst_printer quote_escape)
    name
    (map_atag_A fst (Compile.free_vars_cache (parse_to_anf src ~inject:false)))
    expected ~cmp:(equal_aexpr StringSet.equal)
;;
