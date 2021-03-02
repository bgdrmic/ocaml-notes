open Vjezba7
open OUnit2

let rec string_of_list = function 
    | [] -> ""
    | h::t -> string_of_int h ^ " " ^ string_of_list t

let ounit = QCheck_runner.to_ounit2_test (QCheck.Test.make q is_even ~name:"list")

let tests = "ounit test" >::: [ounit]

let _ = run_test_tt_main tests

