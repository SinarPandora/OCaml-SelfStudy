open OUnit2
open Hello_ocaml.Maps

let binding_test name exp input = name >:: (fun _ -> assert_equal exp (AssocListMap.bindings input))

let assoc_tests =
  let open AssocListMap in
  [
    binding_test "空 map 不包含任何绑定" [] empty;
    (let lst = [(3110, "fun")] in
     binding_test "只有一个键的 map，他的绑定也只有一条" lst (lst |> of_list)
    );
  ]

let suite = "maps suite" >::: assoc_tests

let _ = run_test_tt_main suite
