(** Abstraction function: the list [a1; ... ; an] represents set
    {a1, ... , an}. [] represents empty set.
    Representation invariant: elements of the list are distinct. *)
type t = int list

let rep_ok s = 
  let rec aux x = function
    | [] -> s
    | h::t -> if x >= h then failwith ("rep not OK") else aux h t in
  match s with
  | [] -> []
  | h::t -> aux h t


let empty = []

let size = List.length

let insert x s = 
  let rec aux x = function
    | [] -> [x]
    | h::t ->
      if x = h then h::t
      else if x < h then x::h::t
      else h::(aux x t) in
  rep_ok (aux x s)

let member = List.mem

let rec append_reverse l1 l2 = 
  match l1 with
  | [] -> l2
  | h::t -> append_reverse t (h::l2)

let remove x s = 
  let rec aux x l acc =
    match l with
    | [] -> s
    | h::t ->
      if x < h then aux x t (h::acc)
      else if x = h then append_reverse acc t 
      else s in
  rep_ok (aux x s [])

let choose = function
  | [] -> None
  | h::_ -> Some h

let to_list s = s

let to_string s = List.fold_left (fun a b -> b ^ " " ^a) "" s