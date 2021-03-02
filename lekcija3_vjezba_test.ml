open OUnit2
open Lekcija3_vjezba


let test_product name expected_outcome input =
    name >:: (fun _ -> assert_equal expected_outcome (product input) ~printer:string_of_int)

let test_sort_desc name expected_outcome input =
    name >:: (fun _ -> assert_equal expected_outcome (sort_desc input))

let test_fifth_element name expected_outcome input = 
    name >:: (fun _ -> assert_equal expected_outcome (fifth_element input))


let product_tests = "test suite for product of int list function" >::: [
    test_product "empty" 1 [];
    test_product "one" 1 [1];
    test_product "one two" 2 [1; 2];
    test_product "one two three four five" 120 [1; 2; 3; 4; 5];
]

let sort_desc_tests = "test suite for sort descending function" >::: [
    test_sort_desc "[]" [] [];
    test_sort_desc "[1; 3; 2]" [3; 2; 1] [1; 3; 2];
    test_sort_desc "[4; 3; 2; 1]" [4; 3; 2; 1] [4; 3; 2; 1];
    test_sort_desc "[1; 2; 3; 4]" [4; 3; 2; 1] [1; 2; 3; 4];
]

let fifth_element_tests = "test suite for fifth element of a list function" >::: [
    test_fifth_element "[]" 0 [];
    test_fifth_element "[1; 2; 3]" 0 [1; 2; 3];
    test_fifth_element "[1; 2; 3; 4; 5]" 5 [1; 2; 3; 4; 5];
    test_fifth_element "[1; 2; 3; 4; 5; 6]" 5 [1; 2; 3; 4; 5; 6];
]


let _ = run_test_tt_main product_tests

let _ = run_test_tt_main fifth_element_tests

let _ = run_test_tt_main sort_desc_tests

(* returns:  [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
let rec from i j l =
  if i > j then l
  else from i (j - 1) (j :: l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *) 
let (--) i j =
  from i j []

let longlist = 0 -- 1_000_000
let longlist2 = 500_000 -- 1_000_000
let longlist3 = 0 -- 499_999



let test_drop name expected_outcome outcome = 
    name >:: (fun _ -> assert_equal expected_outcome outcome)

let drop_tests = "test suite for sort descending function" >::: [
    test_drop "3 [1; 2; 3]" [] (drop 3 [1; 2; 3]);
    test_drop "2 [1; 2; 3]" [3] (drop 2 [1; 2; 3]);
    test_drop "500_000 0 -- 1_000_000" longlist2 (drop 500_000 longlist);
]

let test_take name expected_outcome outcome = 
    name >:: (fun _ -> assert_equal expected_outcome outcome)

let take_tests = "test suite for sort descending function" >::: [
    test_take "3 [1; 2; 3]" [1; 2; 3] (take 3 [1; 2; 3]);
    test_take "2 [1; 2; 3]" [1; 2] (take 2 [1; 2; 3]);
    test_take "500_000 0 -- 1_000_000" longlist3 (take 500_000 longlist);
]

let _ = run_test_tt_main drop_tests

let _ = run_test_tt_main take_tests


let list_max_tests = "list_max test suite" >:::
[
    "[]"    >:: (fun _ -> assert_raises (Failure "list_max") (fun _ -> list_max []));
    "[1; 3; 5; 4; 2]" >:: (fun _ -> assert_equal  5 (list_max [1; 3; 5; 4; 2]));
]

let _ = run_test_tt_main list_max_tests