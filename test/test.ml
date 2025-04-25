open Compile
open Exprs
open OUnit2
open Runner
open Utils
open Testing_helper

let builtins_size = 4 * 3

let test_prog =
  "let x = if sub1(55) < 54: (if 1 > 0: add1(2) else: add1(3)) else: (if 0 == 0: sub1(4) else: \
   sub1(5)) in x"
;;

let values =
  "values"
  >::: [ t "num_pos" "5" "" "5";
         t "num_neg" "-15" "" "-15";
         t "num_one" "-1" "" "-1";
         terr "num_static_overflow" "4611686018427387904" ""
           "The number literal 4611686018427387904, used at <num_static_overflow, 1:0-1:19>, is \
            not supported in this language";
         terr "num_static_underflow" "-4611686018427387905" ""
           "The number literal -4611686018427387905, used at <num_static_underflow, 1:0-1:20>, is \
            not supported in this language";
         t "bool_true" "true" "" "true";
         t "bool_false" "false" "" "false";
         t "tup_nil" "nil" "" "nil";
         t "tup_empty" "()" "" "()";
         t "tup_single_num" "(1,)" "" "(1,)";
         t "tup_single_bool" "(true,)" "" "(true,)"
         (* TODO: add more examples of tuples *)
         (* t "func" "(lambda (): 1)" "" "<function "; *)
         (* idk why doesn't work :( *)
         (* t "func_id" "(lambda (x): x)" "" "<function 0x00007f196c93c075>" *) ]
;;

let arith =
  "arithmetic"
  >::: [ t "add1_pos" "add1(15)" "" "16";
         t "add1_neg" "add1(-15)" "" "-14";
         terr "add1_bool" "add1(false)" "" "arithmetic expected number, recieved: false";
         terr "add1_nil" "add1(nil)" "" "arithmetic expected number, recieved: nil";
         terr "add1_tup" "add1((1, 2, 3))" "" "arithmetic expected number, recieved: (1, 2, 3)";
         terr "add1_overflow" "add1(4611686018427387903)" ""
           "Error 5: arithmetic operation overflowed";
         t "addition_pos_pos" "37 + 42" "" "79";
         terr "sub1_bool" "sub1(true)" "" "";
         terr "sub1_bool" "sub1(true)" "" "";
         terr "sub1_underflow" "sub1(-4611686018427387904)" ""
           "Error 5: arithmetic operation overflowed";
         t "gt_true" "2 > 1" "" "true";
         t "gt_false" "1 > 2" "" "false";
         t "gt_equal" "1 > 1" "" "false";
         t "gte_true" "2 >= 1" "" "true";
         t "gte_false" "1 >= 2" "" "false";
         t "gte_equal" "1 >= 1" "" "true";
         t "lte_equal" "1 <= 1" "" "true";
         t "lte_false" "2 <= -3" "" "false";
         t "lt_false" "2 < 1" "" "false";
         t "lt_true" "2 < 3" "" "true";
         t "lt_equal" "1 < 1" "" "false";
         terr "gt_bool" "false > true" "" "comparison expected number, recieved: false";
         t "plus_pos" "7689 + 3360" "" "11049";
         t "mul_simpl" "2 * 2" "" "4";
         terr "times_overflow" "4611686018427387903 * 4611686018427387903" ""
           "Error 5: arithmetic operation overflowed";
         t "nested_bin" "((1 - 2) * (3 + 4))" "" "-7";
         terr "minus_bool" "1016110749334051684 - false" ""
           "arithmetic expected number, recieved: false";
         terr "minus_undeflow" "-4611686018427387903 - 2" "" "arithmetic operation overflowed";
         terr "plus_overflow" "4611686018427387902 + 2" "" "arithmetic operation overflowed";
         t "basic_prims" "add1(2) * sub1(8)" "" "21" (* TODO: all ops *) ]
;;

let logic =
  "logic"
  >::: [ terr "and_num" "3 && 4" "" "Error 3: logic operation expected boolean, recieved: 3";
         terr "or_num" "28 || print(add1(-55))" ""
           "Error 3: logic operation expected boolean, recieved: 28";
         terr "and_mixed_l" "4 && true" "" "Error 3: logic operation expected boolean, recieved: 4";
         terr "and_mixed_r" "true && 10" ""
           "Error 3: logic operation expected boolean, recieved: 10";
         terr "or_mixed_l" "4 || true" "" "Error 3: logic operation expected boolean, recieved: 4";
         terr "or_mixed_r" "false || 10" ""
           "Error 3: logic operation expected boolean, recieved: 10";
         terr "not_num" "!(2)" "" "Error 3: logic operation expected boolean, recieved: 2";
         t "not_false" "!(false)" "" "true";
         t "not_true" "!(true)" "" "false";
         t "if_then" "if true: 5 else: 20" "" "5";
         t "if_else" "if false: 5 else: 20" "" "20";
         t "if_advoid_err" "if true: 3 else: add1(false)" "" "3";
         terr "if_num" "if 0: 5 else: 20" "" "if expected condition to be boolean, recieved: 0";
         terr "if_big_num" "if 4611686018427387903: true else: false" ""
           "if expected condition to be boolean, recieved: 4611686018427387903";
         terr "if_nil" "if nil: 0 else: 1" "" "if expected condition to be boolean, recieved: nil";
         terr "if_tup" "if (1, 2): 0 else: 1" ""
           "if expected condition to be boolean, recieved: (1, 2)";
         t "short_circuit_and" "false && print(true)" "" "false";
         t "short_circuit_or" "true || print(false)" "" "true";
         t "short_circuit_or_no_err" "true || 3" "" "true" ]
;;

let basic_functionality =
  "basic_functionality"
  >::: [ values;
         arith;
         logic;
         t "seq_nums" "1; 2" "" "2";
         t "seq_effect" "print(false); 3" "" "false\n3" ]
;;

let given_tup =
  [ t "given_tup1"
      "let t = (4, (5, 6)) in\n\
      \            begin\n\
      \              t[0] := 7;\n\
      \              t\n\
      \            end"
      "" "(7, (5, 6))";
    t "given_tup2"
      "let t = (4, (5, nil)) in\n\
      \            begin\n\
      \              t[1] := nil;\n\
      \              t\n\
      \            end"
      "" "(4, nil)";
    t "given_tup3"
      "let t = (4, (5, nil)) in\n\
      \            begin\n\
      \              t[1] := t;\n\
      \              t\n\
      \            end"
      "" "(4, <cyclic tuple 1>)";
    t "given_tup4" "let t = (4, 6) in\n            (t, t)" "" "((4, 6), (4, 6))" ]
;;

let tuples =
  "tuples"
  >::: given_tup
       @ [ t "empty" "()" "" "()";
           terr "empty_idx" "()[0]" ""
             "Error 8: index too large: tuple index out of bounds on tuple () of length 0, index: 0";
           terr "empty_idx_let" "let t = () in t[0]" ""
             "Error 8: index too large: tuple index out of bounds on tuple () of length 0, index: 0";
           terr "empty_assgn" "let t = () in t[0] := 0" ""
             "Error 8: index too large: tuple index out of bounds on tuple () of length 0, index: 0";
           t "idx_single" "(1,)[0]" "" "1";
           t "idx_normal1" "(1, 2, 3)[0]" "" "1";
           t "idx_normal2" "(1, 2, 3)[1]" "" "2";
           t "idx_normal3" "(1, 2, 3)[2]" "" "3";
           terr "idx_oob" "(1, 2, 3)[3]" ""
             "Error 8: index too large: tuple index out of bounds on tuple (1, 2, 3) of length 3, \
              index: 3";
           t "assgn_imm" "(1,)[0] := 2" "" "2";
           terr "idx_bool" "(true,)[true]" "" "Error 9: tuple access with non-numeric index: true";
           terr "idx_tup" "(0, 1, 2)[(0,)]" "" "Error 9: tuple access with non-numeric index: (0,)";
           terr "nil_deref" "nil[0]" ""
             "Error 10: access component of nil: attempted to dereference nil";
           terr "nil_assgn" "nil[0] := 1" ""
             "Error 10: access component of nil: attempted to dereference nil";
           t "pair_fst" "(1, 2)[0]" "" "1";
           terr "idx_neg" "(0, 1, 2)[-1]" ""
             "Error 7: index too small: tuple access on negative index: -1";
           t "deep_assign" "let t = ((0,),) in t[0][0] := 1; t" "" "((1,),)";
           t "deep_get" "let t = ((0,),) in t[0][0] := 1; t[0][0]" "" "1";
           t "multi_set" "let t = (0, 0, 0) in t[0] := 1; t[1] := 2; t[2] := 3; t" "" "(1, 2, 3)";
           t "multi_return" "let t = (0, 0, 0) in t[0] := 1; t[1] := 2; t[2] := 3" "" "3" ]
;;

let functions =
  "functions"
  >::: [ t "fn_0_args" "def three(): 3 \n three()" "" "3";
         t "fn_1_args" "def foo(a): a \n foo(1)" "" "1";
         t "fn_2_args" "def foo(a, b): a + b \n foo(1, 1)" "" "2";
         t "fn_3_args" "def foo(a, b, c): a + b + c\n foo(1, 1, 1)" "" "3";
         t "fn_4_args" "def foo(a, b, c, d): a + b + c + d\n foo(1, 1, 1, 1)" "" "4";
         t "fn_5_args" "def foo(a, b, c, d, e): a + b + c + d + e\n foo(1, 1, 1, 1, 1)" "" "5";
         t "fn_6_args" "def foo(a, b, c, d, e, f): a + b + c + d + e + f\n foo(1, 1, 1, 1, 1, 1)" ""
           "6";
         t "fn_7_args"
           "def foo(a, b, c, d, e, f, g): a + b + c + d + e + f + g\n foo(1, 1, 1, 1, 1, 1, 1)" ""
           "7";
         t "fn_8_args"
           "def foo(a, b, c, d, e, f, g, h): a + b + c + d + e + f + g + h\n\
           \ foo(1, 1, 1, 1, 1, 1, 1, 1)"
           "" "8";
         t "fn_9_args"
           "def foo(a, b, c, d, e, f, g, h, i): a + b + c + d + e + f + g + h + i\n\
           \ foo(1, 1, 1, 1, 1, 1, 1, 1, 1)"
           "" "9";
         t "fn_10_args"
           "def foo(a, b, c, d, e, f, g, h, i, j): a + b + c + d + e + f + g + h + i + j\n\
           \ foo(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)"
           "" "10";
         t "fn_ret_1st" "def foo(a, b, c): a \n foo(1, 2, 3)" "" "1";
         t "fn_ret_2nd" "def foo(a, b, c): b \n foo(1, 2, 3)" "" "2";
         t "fn_ret_3rd" "def foo(a, b, c): c \n foo(1, 2, 3)" "" "3";
         terr "fn_wrong_arity_call_over" "def foo(): 1 \n foo(1)" ""
           "Error 17: tried to call a 0-argument function with 1 arguments";
         terr "fn_wrong_arity_call_over2" "def foo(): 1 \n foo(1, 1)" ""
           "Error 17: tried to call a 0-argument function with 2 arguments";
         terr "fn_wrong_arity_call_over3" "def x(a): a\nx(1, 2)\n" ""
           "Error 17: tried to call a 1-argument function with 2 arguments";
         terr "fn_wrong_arity_call_under" "def foo(a, b): a + b \n foo(1)" ""
           "Error 17: tried to call a 2-argument function with 1 arguments";
         terr "fn_wrong_arity_call_under2" "def x(a): a\nx()\n" ""
           "Error 17: tried to call a 1-argument function with 0 arguments";
         t "call_seq" "def foo(a): a + 1 \n def bar(a): a * 5\n foo(5); bar(5)" "" "25";
         t "call_chain" "def foo(a): a + 1 \n def bar(a): a * 5\n foo(bar(7))" "" "36";
         t "call_chain_let"
           "let foo = (lambda (a): a + 1), bar = (lambda (a): a * 5) in foo(bar(7))" "" "36";
         t "call_chain_let2"
           "let x = 3, foo = (lambda (a): a + 1), bar = (lambda (a): foo(a + x)), y = foo(bar(7)) \
            in y"
           "" "12" ]
;;

let lambdas =
  "lambdas"
  >::: [ t "id_call_bool" "(lambda (x): x)(true)" "" "true";
         t "closure" "let x = 5 in (lambda (y): x + y)(7)" "" "12";
         t "stolen_from_notes"
           "let applyToFive = (lambda (it): it(5)) in\n\
            let incr = (lambda (x): x + 1) in\n\
            applyToFive(incr)"
           "" "6";
         t "stolen_from_notes2"
           "let add = (lambda (x): (lambda (y): x + y)) in\n\
            let applyToFive = (lambda (it): it(5)) in\n\
            let incr = add(1) in\n\
            let add5 = add(5) in\n\
            (applyToFive(incr), applyToFive(add5))"
           "" "(6, 10)";
         t "ret_fn" "def foo(x): (lambda: x)\nfoo(2)()" "" "2";
         t "lam_0_args" "(lambda: 3)()" "" "3";
         t "lam_1_args" "(lambda (a): a)(1)" "" "1";
         t "lam_2_args" "(lambda (a, b): a + b)(1, 1)" "" "2";
         t "lam_3_args" "(lambda (a, b, c): a + b + c)(1, 1, 1)" "" "3";
         t "lam_4_args" "(lambda (a, b, c, d): a + b + c + d)(1, 1, 1, 1)" "" "4";
         t "lam_5_args" "(lambda (a, b, c, d, e): a + b + c + d + e)(1, 1, 1, 1, 1)" "" "5";
         t "lam_6_args" "(lambda (a, b, c, d, e, f): a + b + c + d + e + f)(1, 1, 1, 1, 1, 1)" ""
           "6";
         t "lam_7_args"
           "(lambda (a, b, c, d, e, f, g): a + b + c + d + e + f + g)(1, 1, 1, 1, 1, 1, 1)" "" "7";
         t "lam_8_args"
           "(lambda (a, b, c, d, e, f, g, h): a + b + c + d + e + f + g + h)(1, 1, 1, 1, 1, 1, 1, \
            1)"
           "" "8";
         t "lam_9_args"
           "(lambda (a, b, c, d, e, f, g, h, i): a + b + c + d + e + f + g + h + i)(1, 1, 1, 1, 1, \
            1, 1, 1, 1)"
           "" "9";
         t "lam_10_args"
           "(lambda (a, b, c, d, e, f, g, h, i, j): a + b + c + d + e + f + g + h + i + j)(1, 1, \
            1, 1, 1, 1, 1, 1, 1, 1)"
           "" "10";
         t "lam_ret_1st" "(lambda (a, b, c): a)(1, 2, 3)" "" "1";
         t "lam_ret_2nd" "(lambda (a, b, c): b)(1, 2, 3)" "" "2";
         t "lam_ret_3rd" "(lambda (a, b, c): c)(1, 2, 3)" "" "3";
         terr "lam_wrong_arity_call_over" "(lambda: 1)(1)" ""
           "Error 17: tried to call a 0-argument function with 1 arguments";
         terr "lam_wrong_arity_call_over2" "(lambda: 1)(1, 1)" ""
           "Error 17: tried to call a 0-argument function with 2 arguments";
         terr "lam_wrong_arity_call_over3" "(lambda (a): a)(1, 2)" ""
           "Error 17: tried to call a 1-argument function with 2 arguments";
         terr "lam_wrong_arity_call_under" "(lambda (a, b): a + b)(1)" ""
           "Error 17: tried to call a 2-argument function with 1 arguments";
         terr "lam_wrong_arity_call_under2" "(lambda (a): a)()" ""
           "Error 17: tried to call a 1-argument function with 0 arguments";
         terr "call_not_closure" "1()" "" "Error 16: attempted to call a non-function value: 1" ]
;;

let destruct =
  "destruct"
  >::: [ t "destruct_basic_3" "let (x, y, z) = (1, 2, 3) in x + y + z" "" "6";
         t "destruct_nested"
           "let (a, (b, c, (d, (e, f)))) = (1, (2, 3, (4, (5, 6)))) in print(a); print(b); \
            print(c); print(d); print(e); print(f); nil"
           "" "1\n2\n3\n4\n5\n6\nnil";
         terr "destruct_too_few" "let (x) = (1, 2) in nil" ""
           "attempted to destruct a 2-tuple with 1 bindings";
         terr "destruct_too_many" "let (x, y, z) = (1, 2) in nil" ""
           "attempted to destruct a 2-tuple with 3 bindings" ]
;;

let bindings =
  "bindings"
  >::: [ t "blank_nums" "let _ = 1 in 2" "" "2";
         t "blank_effect" "let _ = print(1) in 2" "" "1\n2";
         t "let_in_let" "let a = let b = 3 in add1(b) in a" "" "4" ]
;;

let typeof =
  "typeof"
  >::: [ t "isnum1" "isnum(-33)" "" "true";
         t "isnum2" "isnum(0)" "" "true";
         t "isnum3" "isnum(true)" "" "false";
         t "isnum4" "isnum(false)" "" "false";
         t "isnum5" "isnum(nil)" "" "false";
         t "isnum6" "isnum(())" "" "false";
         t "isbool1" "isbool(true)" "" "true";
         t "isbool2" "isbool(false)" "" "true";
         t "isbool3" "isbool(1)" "" "false";
         t "isbool4" "isbool(2)" "" "false";
         t "isbool5" "isbool(nil)" "" "false";
         t "isbool6" "isbool(())" "" "false";
         t "istuple1" "istuple(())" "" "true";
         t "istuple2" "istuple(nil)" "" "true";
         t "istuple3" "istuple(1)" "" "false";
         t "istuple4" "istuple(true)" "" "false";
         t "istuple5" "istuple(false)" "" "false";
         t "istuple6" "istuple((1, 2))" "" "true";
         t "istuple7" "istuple((true, false))" "" "true" ]
;;

let eq =
  "eq"
  >::: [ t "eq_int1" "1 == 1" "" "true";
         t "eq_int2" "1 == 0" "" "false";
         t "eq_int3" "0 == 1" "" "false";
         t "eq_bool1" "true == true" "" "true";
         t "eq_bool2" "false == false" "" "true";
         t "eq_bool3" "true == false" "" "false";
         t "eq_bool4" "false == true" "" "false";
         t "eq_nil" "nil == nil" "" "true";
         t "eq_tup1" "() == ()" "" "false";
         t "eq_tup2" "(1,) == (1,)" "" "false";
         t "eq_tup3" "(1,2) == (1,2)" "" "false";
         t "eq_tup4" "let x = () in x == x" "" "true";
         t "eq_mixed1" "1 == true" "" "false";
         t "eq_mixed2" "true == 1" "" "false";
         t "eq_mixed3" "() == nil" "" "false";
         t "eq_mixed4" "nil == ()" "" "false";
         t "eq_mixed5" "nil == 1" "" "false";
         t "eq_mixed6" "1 == nil" "" "false";
         t "eq_mixed7" "nil == true" "" "false";
         t "eq_mixed8" "true == nil" "" "false";
         t "eq_mixed9" "true == (1,)" "" "false";
         t "eq_mixed10" "(1,) == true" "" "false";
         t "eq_mixed11" "nil == (1,)" "" "false";
         t "eq_mixed12" "(1,) == nil" "" "false";
         t "eq_mixed13" "(1,) == (1,2)" "" "false";
         t "eq_mixed14" "(1,2) == (1,)" "" "false";
         t "eq_mixed15" "(1,2) == (1,3)" "" "false";
         t "eq_mixed16" "(1,2) == (1,2,3)" "" "false" ]
;;

let equal =
  "equal"
  >::: [ t "equal_int1" "equal(1, 1)" "" "true";
         t "equal_int2" "equal(1, 0)" "" "false";
         t "equal_int3" "equal(0, 1)" "" "false";
         t "equal_bool1" "equal(true, true)" "" "true";
         t "equal_bool2" "equal(false, false)" "" "true";
         t "equal_bool3" "equal(true, false)" "" "false";
         t "equal_bool4" "equal(false, true)" "" "false";
         t "equal_nil" "equal(nil, nil)" "" "true";
         t "equal_tup1" "equal((), ())" "" "true";
         t "equal_tup2" "equal((1,), (1,))" "" "true";
         t "equal_tup3" "equal((1,2), (1,2))" "" "true";
         t "equal_tup4" "equal((1,2), (1,3))" "" "false";
         t "equal_tup5" "equal((1,2), (1,2,3))" "" "false";
         t "equal_tup6" "equal((1,2), (1,))" "" "false";
         t "equal_tup7" "equal((1,), (1,2))" "" "false";
         t "equal_mixed1" "equal(1, true)" "" "false";
         t "equal_mixed2" "equal(true, 1)" "" "false";
         t "equal_mixed3" "equal((), nil)" "" "false";
         t "equal_mixed4" "equal(nil, ())" "" "false";
         t "equal_mixed5" "equal(nil, 1)" "" "false";
         t "equal_mixed6" "equal(1, nil)" "" "false";
         t "equal_mixed7" "equal(nil, true)" "" "false";
         t "equal_mixed8" "equal(true, nil)" "" "false";
         t "equal_mixed9" "equal(true, (1,))" "" "false";
         t "equal_mixed10" "equal((1,), true)" "" "false";
         t "equal_mixed11" "equal(nil, (1,))" "" "false";
         t "equal_mixed12" "equal((1,), nil)" "" "false" ]
;;

let print =
  "print"
  >::: [ t "print_1" "print(1)" "" "1\n1";
         t "print_false" "print(false)" "" "false\nfalse"
         (* TODO: dsjfhaslkdjfhlaksdjhfalksjdhlkajhlkfasdhdlkfajhdflkj *) ]
;;

let input =
  "input"
  >::: [ t "input1" "let x = input() in x + 2" "123" "125";
         t "input_pos_num" "input()" "10002030" "10002030";
         t "input_neg_num" "input()" "-15" "-15";
         t "input_true" "input()" "true" "true";
         t "input_false" "input()" "false" "false";
         (* also print "invalid value entered, try again:" to standard error but 
            these testing functions are bad* *)
         t "input_garbage" "input()" "ඞ\n1" "1";
         t "input_multiple1" "input() + input()" "15 -37" "-22";
         t "input_multiple2" "input() && input()" "true false" "false";
         terr "input_multiple_bad" "input() + input()" "true 3"
           "Error 2: arithmetic expected number, recieved: true" ]
;;

let fdl_given =
  "fdl_given"
  >::: [ t "test_is_bool1" "isbool(true)" "" "true";
         t "test_is_bool2" "isbool(false)" "" "true";
         t "test_is_bool3" "isbool(0)" "" "false";
         t "test_is_bool4" "isbool(123)" "" "false";
         t "test_is_bool5" "isbool((0,123))" "" "false";
         t "test_is_bool6" "isbool((true,123))" "" "false";
         t "test_is_bool7" "isbool((123,123))" "" "false";
         t "test_is_bool8" "isbool((false,123))" "" "false";
         t "test_is_tuple1" "istuple(true)" "" "false";
         t "test_is_tuple2" "istuple(false)" "" "false";
         t "test_is_tuple3" "istuple(0)" "" "false";
         t "test_is_tuple4" "istuple(123)" "" "false";
         t "test_is_tuple5" "istuple((0,123))" "" "true";
         t "test_is_tuple6" "istuple((true,123))" "" "true";
         t "test_is_tuple7" "istuple((123,123))" "" "true";
         t "test_is_tuple8" "istuple((false,123))" "" "true";
         t "test_is_num1" "isnum(true)" "" "false";
         t "test_is_num2" "isnum(false)" "" "false";
         t "test_is_num3" "isnum(0)" "" "true";
         t "test_is_num4" "isnum(123)" "" "true";
         t "test_is_num5" "isnum((0,123))" "" "false";
         t "test_is_num6" "isnum((true,123))" "" "false";
         t "test_is_num7" "isnum((123,123))" "" "false";
         t "test_is_num8" "isnum((false,123))" "" "false";
         terr "scope_err1" "let x = true in (let y = (let x = false in x) in y)" ""
           "shadows one defined";
         t "test" test_prog "" "3";
         (* Some useful if tests to start you off *)
         t "if1" "if 7 < 8: 5 else: 3" "" "5";
         t "if2" "if 0 > 1: 4 else: 2" "" "2";
         terr "overflow" "add1(5073741823000000000)" ""
           "The number literal 5073741823000000000, used at";
         tvg "funcalls" "def fact(n): if n < 2: 1 else: n * fact(n - 1)\n\nfact(5)" "" "120" ]
;;

let free_vars =
  "free_vars"
  >::: [ t_sl "easy" (free_vars (parse_to_anf "x")) ["x"];
         t_sl "medium" (free_vars (parse_to_anf "(lambda (x): x + y)")) ["y"];
         t_sl "hard" (free_vars (parse_to_anf "def x(y): z(y, a)\nand def z(b): x(b)\n1")) ["a"] ]
;;

let oom =
  "out_of_memory"
  >::: [ tgcerr ~dont_elim_dead:true "oomgc1" (builtins_size + 7) "(1, (3, 4))" ""
           "Out of memory: needed 4 words, but only 3 remain after collection";
         tgc "oomgc1_elim" (builtins_size + 7) "(1, (3, 4))" "" "(1, (3, 4))";
         tgc ~dont_elim_dead:true "oomgc2" (builtins_size + 8) "(1, (3, 4))" "" "(1, (3, 4))";
         tvgc ~dont_elim_dead:true "oomgc3" (builtins_size + 8) "(1, (3, 4))" "" "(1, (3, 4))";
         tgc ~dont_elim_dead:true "oomgc4" (builtins_size + 4) "(3, 4)" "" "(3, 4)";
         tgcerr ~dont_elim_dead:true "oomgc5" (builtins_size + 3)
           "(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4)" "" "Allocation" ]
;;

let gc =
  "gc"
  >::: [ tgc "gc_lam1" (builtins_size + 10)
           "let f = (lambda: (1, 2)) in\n\
           \       begin\n\
           \         f();\n\
           \         f();\n\
           \         f();\n\
           \         f()\n\
           \       end"
           "" "(1, 2)" ]
;;

let combined =
  "combined"
  >::: [ t "lam_in_tup1" "((lambda: 1), (lambda: 2))[0]()" "" "1";
         t "lam_in_tup2" "((lambda: 1), (lambda: 2))[1]()" "" "2";
         terr "let_rec_non_fun" "let rec foo = 1 in foo" ""
           "Binding error at let_rec_non_fun, 1:14-1:15: Let-rec expected a name binding to a \
            lambda; got foo";
         terr "let_rec_bad_shadow" "let rec a = (lambda: a), a = (lambda (x): x) in a" ""
           "The identifier a, defined at <let_rec_bad_shadow, 1:25-1:26>, shadows one defined at \
            <let_rec_bad_shadow, 1:8-1:9>";
         t "let_rec_good_shadow" "let rec a = (lambda: a), shadow a = (lambda (x): x) in a(2)" ""
           "2";
         t "mut_closed_tup"
           "let tup = (0,), inc = (lambda: tup[0] := tup[0] + 1) in inc(); inc(); inc()" "" "3";
         t "mut_closed_nested_tup"
           "let tup = ((0,),), inc = (lambda: tup[0][0] := tup[0][0] + 1) in inc(); inc(); inc()" ""
           "3" ]
;;

let fv_cache =
  "free_vars_cache"
  >::: [ t_fv "id" "x" (ACExpr (CImmExpr (ImmId ("x", StringSet.singleton "x"))));
         t_fv "let_bound" "let x = 1 in x"
           (ALet
              ( "x_3",
                CImmExpr (ImmNum (1L, StringSet.empty)),
                ACExpr (CImmExpr (ImmId ("x_3", StringSet.singleton "x_3"))),
                StringSet.empty ) );
         t_fv "let_unbound" "let x = 1 in y"
           (ALet
              ( "x_3",
                CImmExpr (ImmNum (1L, StringSet.empty)),
                ACExpr (CImmExpr (ImmId ("y", StringSet.singleton "y"))),
                StringSet.singleton "y" ) );
         t_fv "let_in_value" "let x = x in x"
           (ALet
              ( "x_3",
                CImmExpr (ImmId ("x", StringSet.singleton "x")),
                ACExpr (CImmExpr (ImmId ("x_3", StringSet.singleton "x_3"))),
                StringSet.singleton "x" ) );
         t_fv "lambda_bound" "(lambda (x): x)"
           (ACExpr
              (CLambda
                 ( ["x_3"],
                   ACExpr (CImmExpr (ImmId ("x_3", StringSet.singleton "x_3"))),
                   StringSet.empty ) ) );
         t_fv "lambda_unbound" "(lambda (x): y)"
           (ACExpr
              (CLambda
                 ( ["x_3"],
                   ACExpr (CImmExpr (ImmId ("y", StringSet.singleton "y"))),
                   StringSet.singleton "y" ) ) );
         t_fv "letrec_bound" "let rec a = (lambda (x): b(x)), b = (lambda (y): a(y, z)) in a(b, x)"
           (ALetRec
              ( [ ( "a_7",
                    CLambda
                      ( ["x_6"],
                        ACExpr
                          (CApp
                             ( ImmId ("b_15", StringSet.singleton "b_15"),
                               [ImmId ("x_6", StringSet.singleton "x_6")],
                               Snake,
                               StringSet.of_list ["x_6"; "b_15"] ) ),
                        StringSet.singleton "b_15" ) );
                  ( "b_15",
                    CLambda
                      ( ["y_14"],
                        ACExpr
                          (CApp
                             ( ImmId ("a_7", StringSet.singleton "a_7"),
                               [ ImmId ("y_14", StringSet.singleton "y_14");
                                 ImmId ("z", StringSet.singleton "z") ],
                               Snake,
                               StringSet.of_list ["a_7"; "y_14"; "z"] ) ),
                        StringSet.of_list ["a_7"; "z"] ) ) ],
                ACExpr
                  (CApp
                     ( ImmId ("a_7", StringSet.singleton "a_7"),
                       [ ImmId ("b_15", StringSet.singleton "b_15");
                         ImmId ("x", StringSet.singleton "x") ],
                       Snake,
                       StringSet.of_list ["a_7"; "b_15"; "x"] ) ),
                StringSet.of_list ["x"; "z"] ) ) ]
;;

let unit_tests = "unit" >::: [fv_cache]

let suite =
  "suite"
  >::: [ unit_tests;
         basic_functionality;
         input;
         tuples;
         functions;
         lambdas;
         destruct;
         oom;
         typeof;
         eq;
         equal;
         fdl_given;
         free_vars;
         gc;
         combined;
         bindings;
         print;
         Testing_tests.simple_parsing_test ]
;;

let () = run_test_tt_main ("all_tests" >::: [suite; input_file_test_suite ()])
