open Qchecks

let odd x = x mod 2 <> 0

let ref_avg lst =
  (float_of_int (List.fold_left (+) 0 lst)) /. (float_of_int (List.length lst))

let odd_div_test = QCheck.Test.make (QCheck.int_range 0 200)
  (fun x -> let y = odd_divisor x in odd y && x mod y = 0)

let max_test = QCheck.(Test.make (pair int int)
  (fun (x, y) ->
    let m = Qchecks.max x y in
    m >= x && m >= y && (m = x || m = y)))

let avg_test = QCheck.(Test.make (list int)
  (fun l ->  l = [] || ref_avg l = Qchecks.avg l))

let _ = QCheck_runner.run_tests [odd_div_test; max_test; avg_test]