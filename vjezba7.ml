let rec is_even = function
  | [] -> false
  | h::t -> h mod 2 = 0 || is_even t

let t = QCheck.Gen.(generate1 (list_size (int_range 5 10) (int_range 0 100)))
let q = QCheck.make QCheck.Gen.(list_size (int_range 5 10) (int_range 0 100))
let intgen = QCheck.int_range 0 100
let p = QCheck.list_of_size (QCheck.Gen.int_range 5 10) (QCheck.int_range 0 100)

