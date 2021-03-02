open OUnit2
open Sets

let make f exp input = fun _ -> assert_equal exp (f input)
let make2 f exp input1 input2 = fun _ -> assert_equal exp (f input1 input2)

let empty = ListSet.empty
let one_two = ListSet.insert 2 (ListSet.insert 1 ListSet.empty)
let one_one_two = ListSet.insert 1 one_two
let one_two_three = ListSet.insert 3 one_two


let size_tests = "test suite for set size function" >::: [

  "empty set []" >:: make ListSet.size 0 empty;
  "normal set [1; 2]" >:: make ListSet.size 2 one_two;
  "added 1 twice [1; 2; 1]" >:: make ListSet.size 2 one_one_two;
]


let choose_tests = "test suite for set choose function" >::: [

  "empty set []" >:: make ListSet.choose None empty;
  "normal set [1; 2]" >:: (fun _ -> assert_equal true
    (match ListSet.choose one_two with
    | Some 1 -> true
    | Some 2 -> true
    | _ -> false));

]

let member_tests = "test suite fo member" >::: [

  "empty set []" >:: make2 ListSet.member false 1 empty;
  "3 in [1; 2]" >:: make2 ListSet.member false 3 one_two;
  "2 in [1; 2]" >:: make2 ListSet.member true 2 one_two;

]


let to_list_tests = "test suite for to_list" >::: [

  "[1;2;3]" >:: (fun _ -> assert_equal [1; 2; 3] (List.sort Stdlib.compare (ListSet.to_list one_two_three))); 
  "[1;2;1]" >:: (fun _ -> assert_equal [1; 2] (List.sort Stdlib.compare (ListSet.to_list one_one_two))); 
  "[]" >:: (fun _ -> assert_equal [] (ListSet.to_list empty)); 

]

let remove_tests = "test suite for remove" >::: [

  "[1; 2; 3] \ 3" >:: (fun _ -> assert_equal [1; 2]
    (List.sort Stdlib.compare (ListSet.to_list (ListSet.remove 3 one_two_three)))); 
  "[1; 2; 3] \ 4" >:: (fun _ -> assert_equal [1; 2; 3]
    (List.sort Stdlib.compare (ListSet.to_list (ListSet.remove 4 one_two_three)))); 
  "[1; 2; 1] \ 1" >:: (fun _ -> assert_equal [2]
    (List.sort Stdlib.compare (ListSet.to_list (ListSet.remove 1 one_one_two)))); 
  
]

let _ = run_test_tt_main choose_tests
let _ = run_test_tt_main size_tests
let _ = run_test_tt_main member_tests
let _ = run_test_tt_main to_list_tests
let _ = run_test_tt_main remove_tests