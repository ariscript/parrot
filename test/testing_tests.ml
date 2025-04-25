open Exprs
open OUnit2
open Testing_helper

let simple_parsing_test =
  "check_parsing"
  >::: [ tparse "check_is_single" "check: 1 is 2. nil"
           (Program
              ( [[DCheck ([CSCheck (Is NEqual, ENumber (1L, ()), ENumber (2L, ()), None, ())], ())]],
                ENil (),
                () ) );
         tparse "check_is_muli" "check: 1 is 2, 3 is 4, 5 is 6. nil"
           (Program
              ( [ [ DCheck
                      ( [ CSCheck (Is NEqual, ENumber (1L, ()), ENumber (2L, ()), None, ());
                          CSCheck (Is NEqual, ENumber (3L, ()), ENumber (4L, ()), None, ());
                          CSCheck (Is NEqual, ENumber (5L, ()), ENumber (6L, ()), None, ()) ],
                        () ) ] ],
                ENil (),
                () ) );
         tparse "check_val" "check: val shadow a = 1. nil"
           (Program
              ([[DCheck ([CSVal (BName ("a", true, ()), ENumber (1L, ()), ())], ())]], ENil (), ())
           );
         tparse "check_vals" "check: val a = 1, val b = 2. nil"
           (Program
              ( [ [ DCheck
                      ( [ CSVal (BName ("a", false, ()), ENumber (1L, ()), ());
                          CSVal (BName ("b", false, ()), ENumber (2L, ()), ()) ],
                        () ) ] ],
                ENil (),
                () ) );
         tparse "check_val_tup" "check: val tup = (1,). nil"
           (Program
              ( [ [ DCheck
                      ([CSVal (BName ("tup", false, ()), ETuple ([ENumber (1L, ())], ()), ())], ())
                  ] ],
                ENil (),
                () ) );
         tparse "check_is_muli_val" "check: val a = 1, val b = 2, a is 1, b is 2. nil"
           (Program
              ( [ [ DCheck
                      ( [ CSVal (BName ("a", false, ()), ENumber (1L, ()), ());
                          CSVal (BName ("b", false, ()), ENumber (2L, ()), ());
                          CSCheck (Is NEqual, EId ("a", ()), ENumber (1L, ()), None, ());
                          CSCheck (Is NEqual, EId ("b", ()), ENumber (2L, ()), None, ()) ],
                        () ) ] ],
                ENil (),
                () ) ) ]
;;

(* 
def even(n):
  if n == 0: true
  else: if n == 1: false
  else: even(n - 2) 

check:
  2 is 2,
  4 satisfies even because 2 * 2.
*)
