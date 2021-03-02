open Assignment1


(* valid_date: Test leap day in a non-century leap year. *)
let () = assert (valid_date 2020 "Feb" 29)

(* valid_date: Test leap day in a century leap year. *)
let () = assert (not (valid_date 2100 "Feb" 29))

(* valid_date: Test leap day in a four-century leap year. *)
let () = assert (valid_date 2000 "Feb" 29)

(* valid_date: Test leap day in a non-leap year. *)
let () = assert (not (valid_date 2001 "Feb" 29))

(* valid_date: Test invalid day. *)
let () = assert (not (valid_date 2010 "Jun" 50))

(* valid_date: Test non-positive year *)
let () = assert (not (valid_date 0 "Jun" 5))

(* valid_date: Test non-positive year *)
let () = assert (not (valid_date (-100) "Jun" 5))

(* syr: Test when n = 1 *)
let () = assert (syr 1 = 0)

(* syr: Test behaviour when n is odd *)
let () = assert (syr 7 - 1 = syr 22)

(* syr: Test behaviour when n is even *)
let () = assert (syr 30 - 1 = syr 15)

(* syr: Test when n = 27 *)
let () = assert (syr 27 = 111)

(* syr: Test when n = 12 *)
let () = assert (syr 12 = 9)

(* nacci: Test when n = 2 *)
let () = assert (nacci 2 6 = [1; 1; 2; 3; 5; 8])

(* nacci: Test when n = 2 *)
let () = assert (nacci 2 6 = [1; 1; 2; 3; 5; 8])

(* nacci: Test when k < 2 *)
let () = assert (nacci 2 1 = [1])

(* nacci: Test when n <> 2 *)
let () = assert (nacci 3 6 = [1; 1; 2; 4; 7; 13])