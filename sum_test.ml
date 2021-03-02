open OUnit2
open Sum

let make_sum_test name expected_output output = 
  name  >:: (fun _ -> assert_equal expected_output (sum output) ~printer:string_of_int)

let tests = "test suite for sum" >::: [
  make_sum_test "empty" 0 [];
  make_sum_test "one" 1 [1];
  make_sum_test "onetwo" 3 [1; 2];
]


let _ = run_test_tt_main tests