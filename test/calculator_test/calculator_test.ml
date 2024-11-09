open OUnit2
open Calculator
open Main

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Int i]. *)
let make_i n i s = n >:: (fun _ -> assert_equal (string_of_int i) (interp s))

(** [make_err n s' s] makes an OUnit test named [n] that expects
    [s] to raise to [Failure s']. *)
let make_err n s' s = n >:: (fun _ -> assert_raises (Failure s') (fun () -> interp s))

let tests =
  [
    make_i "int" 22 "22";
    make_i "add" 22 "11+11";
    make_i "mult" 22 "2*11";
    make_i "let" 22 "let x=22 in x";
    make_i "lets" 22 "let x = 0 in let x = 22 in x";
    make_i "mult of mult" 40 "2*2*10";
    make_i "mult on right of add" 22 "2+2*10";
    make_i "mult on left of add" 14 "2*2+10";
    make_i "nested add" 22 "(10 + 1) + ( 5 + 6 )";
    make_err "unbound" unbound_var_err "x";
  ]

let _ = run_test_tt_main ("suite" >::: tests)
