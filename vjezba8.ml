type student = {mutable gpa: float; name: string}

let alice = {name = "Alice"; gpa = 3.7}

let _ = alice.gpa <- 4.0

let inc = ref (fun x -> x+1)

let f x = if x = 3110 then x else (!inc) (x+1)

let () = inc := f

let (+:=) x y = x := (!x) + y

let x = ref 0
let y = x
let z = ref 0

(* AF: the float array [| x1; ...; xn |] represents the 
 *     vector (x1, ..., xn) 
 * RI: the array is non-empty *)
type vector = float array

let norm v = Array.map (fun x -> x *. x) v |> Array.fold_left (+.) 0. |> sqrt

let normalize v =
  let n = norm v in
  Array.iteri (fun i x -> v.(i) <- x /. n) v


let normalize_loop v =
  let n = norm v in
  for i = 0 to Array.length v - 1 do
  v.(i) <- v.(i) /. n done


let norm_loop v = begin
  let x = ref 0. in
  for i = 0 to Array.length v -1 do
    x := !x +. (v.(i) *. v.(i)) done;

  Stdlib.sqrt (!x)
  end

let init_matrix n m f = begin
  let matrix = Array.make_matrix n m 0 in
  for i = 0 to n-1 do
    for j = 0 to m-1 do
      matrix.(i).(j) <- f i j
    done;
  done;

  matrix
end