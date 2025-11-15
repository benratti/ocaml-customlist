open OUnit2
open Custom_list


let test_length = "test suite for CustomList" >::: [
  "length of empty list" >:: (fun _ ->
    assert_equal 0 (length [])
  );
  "length of single element list" >:: (fun _ ->
    assert_equal 1 (length [42])
  );
  "length of multiple element list" >:: (fun _ ->
    assert_equal 5 (length [1;2;3;4;5])
  );
]

let test_compare_lengths = "test suite for compare_length" >::: [
  "compare two empty lists" >:: (fun _ ->
    assert_equal 0 (compare_lengths [] [])
  );
  "compare empty list with non-empty list" >:: (fun _ ->
    assert_equal (-1) (compare_lengths [] [1;2;3])
  );
  "compare non-empty list with empty list" >:: (fun _ ->
    assert_equal 1 (compare_lengths [1;2;3] [])
  );
  "compare two lists of same length" >:: (fun _ ->
    assert_equal 0 (compare_lengths [1;2] [3;4])
  );
  "compare two lists of different lengths" >:: (fun _ ->
    assert_equal (-1) (compare_lengths [1;2] [3;4;5])
  );
]

let test_compare_length_with = "test suite for compare_length_with" >::: [
  "compare empty list with length 0" >:: (fun _ ->
    assert_equal 0 (compare_length_with [] 0)
  );
  "compare empty list with positive length" >:: (fun _ ->
    assert_equal (-1) (compare_length_with [] 3)
  );
  "compare non-empty list with length 0" >:: (fun _ ->
    assert_equal 1 (compare_length_with [1;2;3] 0)
  );
  "compare list with exact length" >:: (fun _ ->
    assert_equal 0 (compare_length_with [1;2;3] 3)
  );
  "compare list with shorter length" >:: (fun _ ->
    assert_equal 1 (compare_length_with [1;2;3] 2)
  );
  "compare list with longer length" >:: (fun _ ->
    assert_equal (-1) (compare_length_with [1;2;3] 5)
  );
]
let test_is_empty = "test suite for is_empty" >::: [
  "is_empty on empty list" >:: (fun _ ->
    assert_equal true (is_empty [])
  );
  "is_empty on non-empty list" >:: (fun _ ->
    assert_equal false (is_empty [1;2;3])
  );
]

let test_cons = "test suite for cons" >::: [
  "cons on empty list" >:: (fun _ ->
    assert_equal [1] (cons 1 [])
  );
  "cons on non-empty list" >:: (fun _ ->
    assert_equal [0;1;2;3] (cons 0 [1;2;3])
  );
]

let test_hd = "test suite for hd" >::: [
  "hd on single element list" >:: (fun _ ->
    assert_equal 42 (hd [42])
  );
  "hd on multiple element list" >:: (fun _ ->
    assert_equal 1 (hd [1;2;3;4;5])
  );
  "hd on empty list raises exception" >:: (fun _ ->
    assert_raises (Failure "hd: empty list") (fun () -> hd [])
  );
]

let test_tl = "test suite for tl" >::: [
  "tl on single element list" >:: (fun _ ->
    assert_equal [] (tl [42])
  );
  "tl on multiple element list" >:: (fun _ ->
    assert_equal [2;3;4;5] (tl [1;2;3;4;5])
  );
  "tl on empty list raises exception" >:: (fun _ ->
    assert_raises (Failure "tl: empty list") (fun () -> tl [])
  );
]

let test_nth = "test suite for nth" >::: [
  "nth on single element list" >:: (fun _ ->
    assert_equal 42 (nth [42] 0)
  );
  "nth on multiple element list" >:: (fun _ ->
    assert_equal 3 (nth [1;2;3;4;5] 2)
  );
  "nth with out of bounds index raises exception" >:: (fun _ ->
    assert_raises (Failure "nth: index out of bounds") (fun () -> nth [1;2;3] 5)
  );
  "nth with negative index raises exception" >:: (fun _ ->
    assert_raises (Invalid_argument "nth: negative index") (fun () -> nth [1;2;3] (-1))
  );
]

let test_nth_opt = "test suite for nth_opt" >::: [
  "nth_opt on single element list" >:: (fun _ ->
    assert_equal (Some 42) (nth_opt [42] 0)
  );
  "nth_opt on multiple element list" >:: (fun _ ->
    assert_equal (Some 3) (nth_opt [1;2;3;4;5] 2)
  );
  "nth_opt with out of bounds index returns None" >:: (fun _ ->
    assert_equal None (nth_opt [1;2;3] 5)
  );
  "nth_opt with negative index returns None" >:: (fun _ ->
    assert_raises (Invalid_argument "nth: negative index") (fun () -> nth_opt [1;2;3] (-1))
  );
]

let test_rev = "test suite for rev" >::: [
  "rev on empty list" >:: (fun _ ->
    assert_equal [] (rev [])
  );
  "rev on single element list" >:: (fun _ ->
    assert_equal [42] (rev [42])
  );
  "rev on multiple element list" >:: (fun _ ->
    assert_equal [5;4;3;2;1] (rev [1;2;3;4;5])
  );
]
let test_init = "test suite for init" >::: [
  "init with length 0" >:: (fun _ ->
    assert_equal [] (init 0 (fun x -> x))
  );
  "init with length 3" >:: (fun _ ->
    assert_equal [0;1;2] (init 3 (fun x -> x))
  );
  "init with length 5 and function x -> x * x" >:: (fun _ ->
    assert_equal [0;1;4;9;16] (init 5 (fun x -> x * x))
  );
]


let () = run_test_tt_main test_length 
let () = run_test_tt_main test_compare_lengths 
let () = run_test_tt_main test_compare_length_with 
let () = run_test_tt_main test_is_empty
let () = run_test_tt_main test_cons
let () = run_test_tt_main test_hd
let () = run_test_tt_main test_tl
let () = run_test_tt_main test_nth
let () = run_test_tt_main test_nth_opt
let () = run_test_tt_main test_rev
let () = run_test_tt_main test_init