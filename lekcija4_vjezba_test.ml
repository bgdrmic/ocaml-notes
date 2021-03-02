open OUnit2
open Lekcija4_vjezba

let rec string_of_list = function 
    | [] -> ""
    | h::t -> string_of_int h ^ " " ^ string_of_list t

let  string_of_matrix m =
    let rec string_of_matrix_transposed = function
        | [] -> ""
        | h::t -> string_of_list h ^ "\n" ^ string_of_matrix_transposed t
    in string_of_matrix_transposed (transpose m)


let test_fun_1 f name expected_outcome input = 
    name >:: (fun _ -> assert_equal expected_outcome (f input))

let test_fun_2 f name expected_outcome input1 input2 = 
    name >:: (fun _ -> assert_equal expected_outcome (f input1 input2))

let test_fun_1_matrix f name expected_outcome input = 
    name >:: (fun _ -> assert_equal expected_outcome (f input) ~printer:string_of_matrix)

let test_fun_2_matrix f name expected_outcome input1 input2 = 
    name >:: (fun _ -> assert_equal expected_outcome (f input1 input2) ~printer:string_of_matrix)



let add_row_vectors_tests = "test suite for addition of two row vectors" >::: [
    test_fun_2 add_row_vectors "[1; 2; 3] + [1; 1; 1]" [2; 3; 4] [1; 2; 3] [1; 1; 1];
    test_fun_2 add_row_vectors "[1; 2; 3] + [1; 2; 3]" [2; 4; 6] [1; 2; 3] [1; 2; 3];
]

let is_valid_matrix_tests = "test suite for checking whether some int list list is a matrix" >::: [
    test_fun_1 is_valid_matrix "[[1; 2; 3]; [1; 1; 1]]" true [[1; 2; 3]; [1; 1; 1]];
    test_fun_1 is_valid_matrix "[[1; 2; 3]]" true [[1; 2; 3]];
    test_fun_1 is_valid_matrix "[]" false [];
    test_fun_1 is_valid_matrix "[[1; 2; 3]; [1; 1; 1]; [1; 2]]" false [[1; 2; 3]; [1; 1; 1]; [1; 2]];
    test_fun_1 is_valid_matrix "[[]]" false [[]];
    test_fun_1 is_valid_matrix "[[]; []]" false [[]; []];
    test_fun_1 is_valid_matrix "[[1]]" true [[1]];
]

let add_matrices_tests = "test suite for testing addition of two matrices" >::: [
    test_fun_2 add_matrices "[[1; 2; 3]; [1; 1; 1]] + [[1; 1; 1]; [2; 2; 2]]"
        [[2; 3; 4]; [3; 3; 3]] [[1; 2; 3]; [1; 1; 1]] [[1; 1; 1]; [2; 2; 2]];
]

let transpose_tests = "test suite for testing transposition of a matrix" >::: [
    test_fun_1_matrix transpose "[[1; 2; 3]; [1; 1; 1]]" [[1; 1]; [2; 1]; [3; 1]] [[1; 2; 3]; [1; 1; 1]]
]

let multiply_matrices_tests = "test suite for testing multiplication two matrices" >::: [
    test_fun_2_matrix multiply_matrices "[[1; 2; 3]; [1; 1; 1]] * [[1; 1]; [2; 2]; [1; 2]]"
        [[2; 3; 4]; [4; 6; 8]; [3; 4; 5]] [[1; 2; 3]; [1; 1; 1]] [[1; 1]; [2; 2]; [1; 2]];
]

let _ = run_test_tt_main is_valid_matrix_tests
let _ = run_test_tt_main add_row_vectors_tests
let _ = run_test_tt_main add_matrices_tests
let _ = run_test_tt_main transpose_tests
let _ = run_test_tt_main multiply_matrices_tests