open OUnit2
open Custom_list


let () = run_test_tt_main ("test suite for CustomList" >::: [
  "length of empty list" >:: (fun _ ->
    assert_equal 0 (length [])
  );
  "length of single element list" >:: (fun _ ->
    assert_equal 1 (length [42])
  );
  "length of multiple element list" >:: (fun _ ->
    assert_equal 5 (length [1;2;3;4;5])
  );
])

let () = run_test_tt_main ("test suite for compare_length" >::: [
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
])

let () = run_test_tt_main ("test suite for compare_length_with" >::: [
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
])

let () = run_test_tt_main ("test suite for is_empty" >::: [
  "is_empty on empty list" >:: (fun _ ->
    assert_equal true (is_empty [])
  );
  "is_empty on non-empty list" >:: (fun _ ->
    assert_equal false (is_empty [1;2;3])
  );
])

let () = run_test_tt_main ("test suite for cons" >::: [
  "cons on empty list" >:: (fun _ ->
    assert_equal [1] (cons 1 [])
  );
  "cons on non-empty list" >:: (fun _ ->
    assert_equal [0;1;2;3] (cons 0 [1;2;3])
  );
])

let () = run_test_tt_main ("test suite for hd" >::: [
  "hd on single element list" >:: (fun _ ->
    assert_equal 42 (hd [42])
  );
  "hd on multiple element list" >:: (fun _ ->
    assert_equal 1 (hd [1;2;3;4;5])
  );
  "hd on empty list raises exception" >:: (fun _ ->
    assert_raises (Failure "hd: empty list") (fun () -> hd [])
  );
])

let () = run_test_tt_main ("test suite for tl" >::: [
  "tl on single element list" >:: (fun _ ->
    assert_equal [] (tl [42])
  );
  "tl on multiple element list" >:: (fun _ ->
    assert_equal [2;3;4;5] (tl [1;2;3;4;5])
  );
  "tl on empty list raises exception" >:: (fun _ ->
    assert_raises (Failure "tl: empty list") (fun () -> tl [])
  );
])

let () = run_test_tt_main ("test suite for nth" >::: [
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
])

let () = run_test_tt_main ("test suite for nth_opt" >::: [
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
])

let () = run_test_tt_main ("test suite for rev" >::: [
  "rev on empty list" >:: (fun _ ->
    assert_equal [] (rev [])
  );
  "rev on single element list" >:: (fun _ ->
    assert_equal [42] (rev [42])
  );
  "rev on multiple element list" >:: (fun _ ->
    assert_equal [5;4;3;2;1] (rev [1;2;3;4;5])
  );
])

let () = run_test_tt_main ("test suite for init" >::: [
  "init with length 0" >:: (fun _ ->
    assert_equal [] (init 0 (fun x -> x))
  );
  "init with length 3" >:: (fun _ ->
    assert_equal [0;1;2] (init 3 (fun x -> x))
  );
  "init with length 5 and function x -> x * x" >:: (fun _ ->
    assert_equal [0;1;4;9;16] (init 5 (fun x -> x * x))
  );
])

let () = run_test_tt_main ("test suite for append" >::: [
  "append two empty lists" >:: (fun _ ->
    assert_equal [] (append [] [])
  );
  "append empty list to non-empty list" >:: (fun _ ->
    assert_equal [1;2;3] (append [] [1;2;3])
  );
  "append non-empty list to empty list" >:: (fun _ ->
    assert_equal [1;2;3] (append [1;2;3] [])
  );
  "append two non-empty lists" >:: (fun _ ->
    assert_equal [1;2;3;4;5;6] (append [1;2;3] [4;5;6])
  );
])

let () = run_test_tt_main ("test suite for rev_append" >::: [
  "rev_append two empty lists" >:: (fun _ ->
    assert_equal [] (rev_append [] [])
  );
  "rev_append empty list to non-empty list" >:: (fun _ ->
    assert_equal [1;2;3] (rev_append [] [1;2;3])
  );
  "rev_append non-empty list to empty list" >:: (fun _ ->
    assert_equal [3;2;1] (rev_append [1;2;3] [])
  );
  "rev_append two non-empty lists" >:: (fun _ ->
    assert_equal [3;2;1;4;5;6] (rev_append [1;2;3] [4;5;6])
  );
])

let () = run_test_tt_main ("test suite for concat" >::: [
  "concat an empty list" >:: (fun _ ->
    assert_equal [] (concat [])
  );
  "concat list of list " >:: (fun _ ->
    assert_equal [1;2;3;4;5;6;7;8;9] ( concat [[1;2];[];[3;4;5];[6;7;8;9];[]])
  );
])

let () = run_test_tt_main ("test suite for flatten" >::: [
  "flatten an empty list" >:: (fun _ ->
    assert_equal [] (flatten [])
  );
  "flatten list of list " >:: (fun _ ->
    assert_equal [1;2;3;4;5;6;7;8;9] (flatten [[1;2];[];[3;4;5];[6;7;8;9];[]])
  );
])

let () = run_test_tt_main ("test suite for equal" >::: [
  "equal for two empty lists" >:: (fun _ -> 
    assert_bool "empty lists are equal" (equal (fun x y -> x = y) [] [] )
  );
  "equal for two lists with first list is shorter then second list" >:: (fun _ -> 
    assert_bool "lists with different lengths are not equal" (not (equal (fun x y -> x = y) [1;2;3] [1;2;3;4]) )
  );
  "equal for two lists with second list is shorter then first list" >:: (fun _ -> 
    assert_bool "lists with different lengths are not equal" (not (equal (fun x y -> x = y) [1;2;3] [1;2]) )
  );
  "equal for two lists with different elements" >:: (fun _ -> 
    assert_bool "lists with different elements are not equal" (not (equal (fun x y -> x = y) [4;5;6] [1;2;3]) )
  );
])

let () = run_test_tt_main ("test suite for compare" >::: [
  "compare two empty list" >:: (fun _ ->
    assert_equal 0 (compare Int.compare [] [])
  );
  "compare two lists with differents size" >:: (fun _ ->
    assert_equal (-1) (compare Int.compare [1;2] [1;2;3])
  );
  "compare two lists with differents size" >:: (fun _ ->
    assert_equal 1 (compare Int.compare [1;2;3;4] [1;2;3])
  );
  "compare two lists with same siez" >:: (fun _ ->
    assert_equal 1 (compare Int.compare [1;2;5] [1;2;3])
  );
  "compare two lists with same siez" >:: (fun _ ->
    assert_equal (-1) (compare Int.compare [1;2;2] [1;2;3])
  );

])

let () = run_test_tt_main ("test suite for iter" >::: [
    "iter on list" >:: (fun _ -> 
      let d = ref 0 in
      let _ = iter (fun a -> d := !d + a) [1;1;1] in
      assert_equal 3 !d
    );
    "iter on empty list" >:: (fun _ -> 
      let d = ref 0 in
      let _ = iter (fun a -> d := !d + a) [] in
      assert_equal 0 !d
    );
    
])


let () = run_test_tt_main ("test suite for iteri" >::: [
    "iteri on list" >:: (fun _ -> 
      let d = ref 0 in
      let _ = iteri (fun i a -> d := !d + a + i) [1;1;1] in
      assert_equal 6 !d
    );
    "iteri on empty list" >:: (fun _ -> 
      let d = ref 0 in
      let _ = iteri (fun i a -> d := !d + a + i) [] in
      assert_equal 0 !d
    );
    
])

let () = run_test_tt_main ("test suite for map" >::: [
  "map on empty list" >:: ( fun _ ->
    assert_equal [] (map (fun _ -> 0) [])
  );
  "map on list" >:: (fun _ ->
    assert_equal [2;4;6;8] (map (fun x -> 2*x) [1;2;3;4])
  );  
])

let () = run_test_tt_main ("test suite for mapi" >::: [
  "mapi on empty list" >:: ( fun _ ->
    assert_equal [] (mapi (fun _ _ -> 0) [])
  );
  "mapi on list" >:: (fun _ ->
    assert_equal [2;5;8;11] (mapi (fun i x -> 2*x + i) [1;2;3;4])
  );
    
])

let () = run_test_tt_main ("test suite for rev_map" >::: [
  "rev_map on empty list" >:: ( fun _ ->
    assert_equal [] (map (fun _ -> 0) [])
  );
  "rev_map on list" >:: (fun _ ->
    assert_equal [8;6;4;2] (rev_map (fun x -> 2*x) [1;2;3;4])
  );  
])

let () = run_test_tt_main ("test suite for concat_map" >::: [
  "concat_map on empty list" >:: ( fun _ ->
    assert_equal [] (concat_map (fun _ -> []) [])
  );
  "concat_map on list" >:: (fun _ ->
    assert_equal [1;2;3;4;5;6] (concat_map (fun x -> [x; x+1]) [1;3;5])
  );  
])

let () = run_test_tt_main ("test suite for fold_left" >::: [
  "fold_left on empty list" >:: ( fun _ ->
    assert_equal 0 (fold_left (fun acc x -> acc + x) 0 [])
  );
  "fold_left on list" >:: (fun _ ->
    assert_equal "abcd" (fold_left (fun acc s -> acc ^ s) "" ["a";"b";"c";"d"])
  );
    
])

let () = run_test_tt_main ("test suite for fold_right" >::: [
    "fold_right on empty list" >:: ( fun _ ->
      assert_equal 0 (fold_right (fun acc x -> acc + x) [] 0)
    );
    "fold_right on list" >:: ( fun _ -> 
      assert_equal "dcba" (fold_right (fun s acc -> acc ^ s) ["a";"b";"c";"d"] "")
    );
])


let () = run_test_tt_main ("test suite for iter2" >::: [  
  "iter2 on two empty lists" >:: ( fun _ ->
    let acc = ref 0 in
    let _ = iter2 (fun x y -> acc := !acc + x + y) [] [] in
    assert_equal 0 !acc
  );
  "iter2 on two non-empty lists" >:: ( fun _ ->
    let acc = ref 0 in
    let _ = iter2 (fun x y -> acc := !acc + x * y) [1;2;3] [4;5;6] in
    assert_equal 32 !acc  (* 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32 *)
  );
  "iter2 on lists of different lengths raises exception" >:: ( fun _ ->
    assert_raises (Invalid_argument "iter2: lists have different lengths") (fun () ->
      iter2 (fun _ _ -> ()) [1;2] [3]
    )
  );
])

let () = run_test_tt_main ("test suite for map2" >::: [  
  "map2 on two empty lists" >:: ( fun _ ->
    assert_equal [] (map2 (fun x y -> x + y) [] [])
  );
  "map2 on two non-empty lists" >:: ( fun _ ->
    assert_equal [5;7;9] (map2 (fun x y -> x + y) [1;2;3] [4;5;6])
  );
  "map2 on lists of different lengths raises exception" >:: ( fun _ ->
    assert_raises (Invalid_argument "map2: lists have different lengths") (fun () ->
      map2 (fun _ _ -> 0) [1;2] [3]
    )
  );
])

let () = run_test_tt_main ("test suite for rev_map2" >::: [  
  "rev_map2 on two empty lists" >:: ( fun _ ->
    assert_equal [] (rev_map2 (fun x y -> x + y) [] [])
  );
  "rev_map2 on two non-empty lists" >:: ( fun _ ->
    assert_equal [9;7;5] (rev_map2 (fun x y -> x + y) [1;2;3] [4;5;6])
  );
  "rev_map2 on lists of different lengths raises exception" >:: ( fun _ ->
    assert_raises (Invalid_argument "rev_map2: lists have different lengths") (fun () ->
      rev_map2 (fun _ _ -> 0) [1;2] [3]
    )
  );
])


let () = run_test_tt_main ("test suite for fold_left2" >::: [  
  "fold_left2 on two empty lists" >:: ( fun _ ->
    assert_equal 0 (fold_left2 (fun acc x y -> acc + x + y) 0 [] [])
  );
  "fold_left2 on two non-empty lists" >:: ( fun _ ->
    assert_equal 32 (fold_left2 (fun acc x y -> acc + x * y) 0 [1;2;3] [4;5;6])  (* 0 + 1*4 + 2*5 + 3*6 = 0 + 4 + 10 + 18 = 32 *)
  );
  "fold_left2 on lists of different lengths raises exception" >:: ( fun _ ->
    assert_raises (Invalid_argument "fold_left2: lists have different lengths") (fun () ->
      fold_left2 (fun acc _ _ -> acc) 0 [1;2] [3]
    )
  );
])


let () = run_test_tt_main ("test suite for fold_right2" >::: [  
  "fold_right2 on two empty lists" >:: ( fun _ ->
    assert_equal 0 (fold_right2 (fun x y acc -> acc + x + y) [] [] 0)
  );
  "fold_right2 on two non-empty lists" >:: ( fun _ ->
    assert_equal 32 (fold_right2 (fun x y acc -> acc + x * y) [1;2;3] [4;5;6] 0)  (* 3*6 + 2*5 + 1*4 + 0 = 18 + 10 + 4 + 0 = 32 *)
  );
  "fold_right2 on lists of different lengths raises exception" >:: ( fun _ ->
    assert_raises (Invalid_argument "fold_right2: lists have different lengths") (fun () ->
      fold_right2 (fun _ _ acc -> acc) [1;2] [3] 0
    )
  );
])


let () = run_test_tt_main ("test suite for for_all" >::: [  
  "for_all on empty list" >:: ( fun _ ->
    assert_equal true (for_all (fun x -> x > 0) [])
  );
  "for_all on list where all elements satisfy predicate" >:: ( fun _ ->
    assert_equal true (for_all (fun x -> x > 0) [1;2;3;4;5])
  );
  "for_all on list where some elements do not satisfy predicate" >:: ( fun _ ->
    assert_equal false (for_all (fun x -> x > 0) [1;-2;3;4;5])
  );
])

let () = run_test_tt_main ("test suite for exists" >::: [  
  "exists on empty list" >:: ( fun _ ->
    assert_equal false (exists (fun x -> x > 0) [])
  );
  "exists on list where some elements satisfy predicate" >:: ( fun _ ->
    assert_equal true (exists (fun x -> x > 0) [-1;-2;3;-4;-5])
  );
  "exists on list where no elements satisfy predicate" >:: ( fun _ ->
    assert_equal false (exists (fun x -> x > 0) [-1;-2;-3;-4;-5])
  );
])

let () = run_test_tt_main ("test suite for for_all2" >::: [  
  "for_all2 on two empty lists" >:: ( fun _ ->
    assert_equal true (for_all2 (fun x y -> x = y) [] [])
  );
  "for_all2 on two non-empty lists where all pairs satisfy predicate" >:: ( fun _ ->
    assert_equal true (for_all2 (fun x y -> x < y) [1;2;3] [2;3;4])
  );
  "for_all2 on two non-empty lists where some pairs do not satisfy predicate" >:: ( fun _ ->
    assert_equal false (for_all2 (fun x y -> x < y) [1;3;2] [2;3;4])
  );
  "for_all2 on lists of different lengths raises exception" >:: ( fun _ ->
    assert_raises (Invalid_argument "for_all2: lists have different lengths") (fun () ->
      for_all2 (fun _ _ -> true) [1;2] [3]
    )
  );
])  

let () = run_test_tt_main ("test suite for exists2" >::: [  
  "exists2 on two empty lists" >:: ( fun _ ->
    assert_equal false (exists2 (fun x y -> x = y) [] [])
  );
  "exists2 on two non-empty lists where some pairs satisfy predicate" >:: ( fun _ ->
    assert_equal true (exists2 (fun x y -> x + y = 5) [1;2;3] [4;3;2])
  );
  "exists2 on two non-empty lists where no pairs satisfy predicate" >:: ( fun _ ->
    assert_equal false (exists2 (fun x y -> x + y = 0) [1;2;3] [4;5;6])
  );
  "exists2 on lists of different lengths raises exception" >:: ( fun _ ->
    assert_raises (Invalid_argument "exists2: lists have different lengths") (fun () ->
      exists2 (fun _ _ -> false) [1;2] [3]
    )
  );
])

let () = run_test_tt_main ("test suite for mem" >::: [  
  "mem on empty list" >:: ( fun _ ->
    assert_equal false (mem 1 [])
  );
  "mem on list where element is present" >:: ( fun _ ->
    assert_equal true (mem 3 [1;2;3;4;5])
  );
  "mem on list where element is absent" >:: ( fun _ ->
    assert_equal false (mem 6 [1;2;3;4;5])
  );
])

let () = run_test_tt_main ("test suite for memq" >::: [  
  "memq on empty list" >:: ( fun _ ->
    assert_equal false (memq (ref 1) [])
  );
  "memq on list where element is present" >:: ( fun _ ->
    let a = ref 3 in
    let b = ref 4 in
    assert_equal true (memq a [ref 1; ref 2; a; b])
  );
  "memq on list where element is absent" >:: ( fun _ ->
    let a = ref 3 in
    let b = ref 4 in
    assert_equal false (memq a [ref 1; ref 2; b; ref 3])
  );
])

let () = run_test_tt_main ("test suite for find" >::: [  
  "find on empty list raises exception" >:: ( fun _ ->
    assert_raises (Not_found) (fun () -> find (fun x -> x = 1) [])
  );
  "find on list where element is present" >:: ( fun _ ->
    assert_equal 3 (find (fun x -> x = 3) [1;2;3;4;5])
  );
  "find on list where element is absent raises exception" >:: ( fun _ ->
    assert_raises (Not_found) (fun () -> find (fun x -> x = 6) [1;2;3;4;5])
  );
])

let () = run_test_tt_main ("test suite for find_opt" >::: [  
  "find_opt on empty list" >:: ( fun _ ->
    assert_equal None (find_opt (fun x -> x = 1) [])
  );
  "find_opt on list where element is present" >:: ( fun _ ->
    assert_equal (Some 3) (find_opt (fun x -> x = 3) [1;2;3;4;5])
  );
  "find_opt on list where element is absent" >:: ( fun _ ->
    assert_equal None (find_opt (fun x -> x = 6) [1;2;3;4;5])
  );
])

let () = run_test_tt_main ("test suite for find_index" >::: [  
  "find_index on empty list raises exception" >:: ( fun _ ->
    assert_equal None (find_index (fun x -> x = 1) [])
  );
  "find_index on list where element is present" >:: ( fun _ ->
    assert_equal (Some 2) (find_index (fun x -> x = 3) [1;2;3;4;5])
  );
  "find_index on list where element is absent raises exception" >:: ( fun _ ->
    assert_equal None (find_index (fun x -> x = 6) [1;2;3;4;5])
  );
])

let () = run_test_tt_main ("test suite for find_map" >::: [  
  "find_map on empty list" >:: ( fun _ ->
    assert_equal None (find_map (fun x -> Some x) [])
  );
  "find_map on list where function returns Some" >:: ( fun _ ->
    assert_equal (Some 3) (find_map (fun x -> if x = 3 then Some x else None) [1;2;3;4;5])
  );
  "find_map on list where function returns None" >:: ( fun _ ->
    assert_equal None (find_map (fun _ -> None) [1;2;3;4;5])
  );
])

let () = run_test_tt_main ("test suite for find_mapi" >::: [  
  "find_mapi on empty list" >:: ( fun _ ->
    assert_equal None (find_mapi (fun i x -> Some (i, x)) [])
  );
  "find_mapi on list where function returns Some" >:: ( fun _ ->
    assert_equal (Some (2, 3)) (find_mapi (fun i x -> if x = 3 then Some (i, x) else None) [1;2;3;4;5])
  );
  "find_mapi on list where function returns None" >:: ( fun _ ->
    assert_equal None (find_mapi (fun _ _ -> None) [1;2;3;4;5])
  );
] )


let () = run_test_tt_main ("test suite for filter" >::: [  
  "filter on empty list" >:: ( fun _ ->
    assert_equal [] (filter (fun x -> x > 0) [])
  );
  "filter on list where some elements satisfy predicate" >:: ( fun _ ->
    assert_equal [2;4;6] (filter (fun x -> x mod 2 = 0) [1;2;3;4;5;6])
  );
  "filter on list where no elements satisfy predicate" >:: ( fun _ ->
    assert_equal [] (filter (fun x -> x > 10) [1;2;3;4;5])
  );
])

let () = run_test_tt_main ("test suite for find_all" >::: [  
  "find_all on empty list" >:: ( fun _ ->
    assert_equal [] (find_all (fun x -> x > 0) [])
  );
  "find_all on list where some elements satisfy predicate" >:: ( fun _ ->
    assert_equal [2;4;6] (find_all (fun x -> x mod 2 = 0) [1;2;3;4;5;6])
  );
  "find_all on list where no elements satisfy predicate" >:: ( fun _ ->
    assert_equal [] (find_all (fun x -> x > 10) [1;2;3;4;5])
  );
])


let () = run_test_tt_main ("test suite for filteri" >::: [  
  "filteri on empty list" >:: ( fun _ ->
    assert_equal [] (filteri (fun i x -> x > i) [])
  );
  "filteri on list where some elements satisfy predicate" >:: ( fun _ ->
    assert_equal [4;6] (filteri (fun i x -> i >= 3 && x mod 2 = 0) [1;2;3;4;5;6])
  );
  "filteri on list where no elements satisfy predicate" >:: ( fun _ ->
    assert_equal [] (filteri (fun i x -> x > 10 + i) [1;2;3;4;5])
  );
])



let () = run_test_tt_main ("test suite for take" >::: [  
  "take on empty list" >:: ( fun _ ->
    assert_equal [] (take 3 [])
  );
  "take on list with more elements than n" >:: ( fun _ ->
    assert_equal [1;2;3] (take 3 [1;2;3;4;5])
  );
  "take on list with fewer elements than n" >:: ( fun _ ->
    assert_equal [1;2;3;4;5] (take 10 [1;2;3;4;5])
  );
  "take with n = 0" >:: ( fun _ ->
    assert_equal [] (take 0 [1;2;3;4;5])
  );
  "take with negative n raises exception" >:: ( fun _ ->
    assert_raises (Invalid_argument "take: negative count") (fun () -> take (-1) [1;2;3])
  );
])

let () = run_test_tt_main ("test suite for drop" >::: [  
  "drop on empty list" >:: ( fun _ ->
    assert_equal [] (drop 3 [])
  );
  "drop on list with more elements than n" >:: ( fun _ ->
    assert_equal [4;5] (drop 3 [1;2;3;4;5])
  );
  "drop on list with fewer elements than n" >:: ( fun _ ->
    assert_equal [] (drop 10 [1;2;3;4;5])
  );
  "drop with n = 0" >:: ( fun _ ->
    assert_equal [1;2;3;4;5] (drop 0 [1;2;3;4;5])
  );
  "drop with negative n raises exception" >:: ( fun _ ->
    assert_raises (Invalid_argument "drop: negative count") (fun () -> drop (-1) [1;2;3])
  );
])

let () = run_test_tt_main ("test suite for take_while" >::: [  
  "take_while on empty list" >:: ( fun _ ->
    assert_equal [] (take_while (fun x -> x > 0) [])
  );
  "take_while on list where some elements satisfy predicate" >:: ( fun _ ->
    assert_equal [1;2;3] (take_while (fun x -> x < 4) [1;2;3;4;5])
  );
  "take_while on list where no elements satisfy predicate" >:: ( fun _ ->
    assert_equal [] (take_while (fun x -> x > 10) [1;2;3;4;5])
  );

])

let () = run_test_tt_main ("test suite for drop_while" >::: [  
  "drop_while on empty list" >:: ( fun _ ->
    assert_equal [] (drop_while (fun x -> x > 0) [])
  );
  "drop_while on list where some elements satisfy predicate" >:: ( fun _ ->
    assert_equal [4;5] (drop_while (fun x -> x < 4) [1;2;3;4;5])
  );
  "drop_while on list where all elements satisfy predicate" >:: ( fun _ ->
    assert_equal [] (drop_while (fun x -> x > 0) [1;2;3;4;5])
  );
])

let () = run_test_tt_main ("test suite for partition" >::: [  
  "partition on empty list" >:: ( fun _ ->
    assert_equal ([], []) (partition (fun x -> x > 0) [])
  );
  "partition on list with mixed elements" >:: ( fun _ ->
    assert_equal ([2;4;6], [1;3;5]) (partition (fun x -> x mod 2 = 0) [1;2;3;4;5;6])
  );
  "partition on list where all elements satisfy predicate" >:: ( fun _ ->
    assert_equal ([1;2;3], []) (partition (fun x -> x > 0) [1;2;3])
  );
  "partition on list where no elements satisfy predicate" >:: ( fun _ ->
    assert_equal ([], [-1;-2;-3]) (partition (fun x -> x > 0) [-1;-2;-3])
  );
])

let () = run_test_tt_main ("test suite for partition_map" >::: [  
  "partition_map on empty list" >:: ( fun _ ->
    assert_equal ([], []) (partition_map (fun x -> if x > 0 then Either.Left x else Either.Right x) [])
  );
  "partition_map on list with mixed elements" >:: ( fun _ ->
    assert_equal ([2;4;6], [1;3;5]) (partition_map (fun x -> if x mod 2 = 0 then Either.Left x else Either.Right x) [1;2;3;4;5;6])
  );
  "partition_map on list where all elements go to Left" >:: ( fun _ ->
    assert_equal ([-1;-2;-3], []) (partition_map (fun x -> Either.Left (-x)) [1;2;3])
  );
  "partition_map on list where all elements go to Right" >:: ( fun _ ->
    assert_equal ([], [1;2;3]) (partition_map (fun x -> Either.Right (-x)) [-1;-2;-3])
  );
])

let () = run_test_tt_main ("test suite for assoc" >::: [  
  "assoc on empty list raises exception" >:: ( fun _ ->
    assert_raises (Not_found) (fun () -> assoc 1 [])
  );
  "assoc on list where key is present" >:: ( fun _ ->
    assert_equal "b" (assoc 2 [(1,"a");(2,"b");(3,"c")])
  );
  "assoc on list where key is absent raises exception" >:: ( fun _ ->
    assert_raises (Not_found) (fun () -> assoc 4 [(1,"a");(2,"b");(3,"c")])
  );
])

let () = run_test_tt_main ("test suite for assoc_opt" >::: [  
  "assoc_opt on empty list" >:: ( fun _ ->
    assert_equal None (assoc_opt 1 [])
  );
  "assoc_opt on list where key is present" >:: ( fun _ ->
    assert_equal (Some "b") (assoc_opt 2 [(1,"a");(2,"b");(3,"c")])
  );
  "assoc_opt on list where key is absent" >:: ( fun _ ->
    assert_equal None (assoc_opt 4 [(1,"a");(2,"b");(3,"c")])
  );
])

let () = run_test_tt_main ("test suite for assq" >::: [  
  "assq on empty list raises exception" >:: ( fun _ ->
    assert_raises (Not_found) (fun () -> assq (ref 1) [])
  );
  "assq on list where key is present" >:: ( fun _ ->
    let a = ref 2 in
    let b = ref 3 in
    assert_equal "b" (assq a [(ref 1,"a");(a,"b");(b,"c")])
  );
  "assq on list where key is absent raises exception" >:: ( fun _ ->
    let a = ref 4 in
    assert_raises (Not_found) (fun () -> assq a [(ref 1,"a");(ref 2,"b");(ref 3,"c");(ref 4,"d")])
  );
])

let () = run_test_tt_main ("test suite for assq_opt" >::: [  
  "assq_opt on empty list" >:: ( fun _ ->
    assert_equal None (assq_opt (ref 1) [])
  );
  "assq_opt on list where key is present" >:: ( fun _ ->
    let a = ref 2 in
    let b = ref 3 in
    assert_equal (Some "b") (assq_opt a [(ref 1,"a");(a,"b");(b,"c")])
  );
  "assq_opt on list where key is absent" >:: ( fun _ ->
    let a = ref 4 in
    assert_equal None (assq_opt a [(ref 1,"a");(ref 2,"b");(ref 3,"c");(ref 4,"d")])
  );
])


let () = run_test_tt_main ("test suite for mem_assoc" >::: [  
  "mem_assoc on empty list" >:: ( fun _ ->
    assert_equal false (mem_assoc 1 [])
  );
  "mem_assoc on list where key is present" >:: ( fun _ ->
    assert_equal true (mem_assoc 2 [(1,"a");(2,"b");(3,"c")])
  );
  "mem_assoc on list where key is absent" >:: ( fun _ ->
    assert_equal false (mem_assoc 4 [(1,"a");(2,"b");(3,"c")])
  );
])

let () = run_test_tt_main ("test suite for mem_assq" >::: [  
  "mem_assq on empty list" >:: ( fun _ ->
    assert_equal false (mem_assq (ref 1) [])
  );
  "mem_assq on list where key is present" >:: ( fun _ ->
    let a = ref 2 in
    let b = ref 3 in
    assert_equal true (mem_assq a [(ref 1,"a");(a,"b");(b,"c")])
  );
  "mem_assq on list where key is absent" >:: ( fun _ ->
    let a = ref 4 in
    assert_equal false (mem_assq a [(ref 1,"a");(ref 2,"b");(ref 3,"c");(ref 4,"d")])
  );
])

let () = run_test_tt_main ("test suite for remove_assq" >::: [  
  "remove_assq on empty list" >:: ( fun _ ->
    assert_equal [] (remove_assq (ref 1) [])
  );
  "remove_assq on list where key is present" >:: ( fun _ ->
    let a = ref 2 in
    let b = ref 3 in
    assert_equal [(ref 1,"a");(b,"c")] (remove_assq a [(ref 1,"a");(a,"b");(b,"c")])
  );
  "remove_assq on list where key is absent" >:: ( fun _ ->
    let a = ref 4 in
    assert_equal [(ref 1,"a");(ref 2,"b");(ref 3,"c");(ref 4,"d")] (remove_assq a [(ref 1,"a");(ref 2,"b");(ref 3,"c");(ref 4,"d")])
  );
])

let () = run_test_tt_main ("test suite for split" >::: [  
  "split on empty list" >:: ( fun _ ->
    assert_equal ([], []) (split [])
  );
  "split on non-empty list" >:: ( fun _ ->
    assert_equal ([1;2;3], ["a";"b";"c"]) (split [(1,"a");(2,"b");(3,"c")])
  );
])

let () = run_test_tt_main ("test suite for combine" >::: [  
  "combine on two empty lists" >:: ( fun _ ->
    assert_equal [] (combine [] [])
  );
  "combine on two non-empty lists of same length" >:: ( fun _ ->
    assert_equal [(1,"a");(2,"b");(3,"c")] (combine [1;2;3] ["a";"b";"c"])
  );
  "combine on lists of different lengths raises exception" >:: ( fun _ ->
    assert_raises (Invalid_argument "combine: lists have different lengths") (fun () ->
      combine [1;2] ["a";"b";"c"]
    )
  );
])