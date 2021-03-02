(** [Poly] represents immutable polynomials with integer coefficients. *)
module type Poly = sig
  (** [t] is the type of polynomials *)
  type t

  (** [eval x p] is [p] evaluated at [x].  
      Example:  if [p] represents $3x^3 + x^2 + x$, then 
      [eval 10 p] is [3110]. *)
  val eval : int -> t -> int

  (** [make lst] is a polynomial $an*x^n + ... + a0$
      where [lst] is [an; ... ; a0]
      Example: if [lst] is [2; 0; 1], then
      [make lst] represents polynomial $2x^2 + 1$ *)
  val make : int list -> t

  (** [to_string p] is string represantation of [p] 
      Example: if [p] represents $x^2 + 2$
      then [to_string p] is "x^2 + 2"
      [to_string [] is "0" *)
  val to_string : t -> string 

  (** [degree p] is a degree of [p]
      Degree of $x^2 + x + 1$ is 2 *)
  val degree : t -> int


  (** [add p q] is polynomial [p + q] *)
  val add : t -> t -> t

  (** [mul p q] is polynomial [p * q]
      where [p] and [q] are polynomials *)
  val mul : t -> t -> t

  (** [add_int a p] is integer [a] added to polynomial [p] *)
  val add_int : int -> t -> t

  (** [mul_int a p] is a polynomial [p]  multiplyed by integer [a] *)
  val mul_int : int -> t -> t
end

module PolyImpl : Poly = struct

  type t = int list

  let rec trim = function
  | [] -> []
  | h::t -> if h = 0 then trim t else h::t

  let make lst = trim (List.rev lst)

  let degree p = List.length p - 1

  let to_string_mono a n =
    if a = 0 then (if n = 0 then "0" else "")
    else if n = 0 then string_of_int a
    else if n = 1 then string_of_int a ^ "x"
    else string_of_int a ^ "x^" ^ string_of_int n

  let to_string p =
    let rec to_string_aux n acc = function
      | [] -> "0"
      | h::[] -> to_string_mono h n ^ acc
      | h::t -> to_string_aux (n+1) (" + " ^ to_string_mono h n) t in
    to_string_aux 0 "" p

  let eval x p =
    let rec eval_aux x acc = function
      | [] -> 0
      | h::t -> eval_aux x (acc*x + h) t in 
    eval_aux x 0 (List.rev p)

  let add p q =
    if p = [] then q
    else if q = [] then p
    else
      let rec add_aux p q acc =
        match (p, q) with
        | ([], []) -> acc
        | (h1::t1, []) -> add_aux t1 [] (h1::acc)
        | ([], q) -> add_aux q [] acc
        | (h1::t1, h2::t2) -> add_aux t1 t2 ((h1+h2)::acc) in
      List.rev (trim (add_aux p q []))

  let add_int a = function
    | [] -> [a]
    | h::t -> trim((a+h)::t)

  let rec mul_int a p =
    if a = 0 then []
    else match p with
    | [] -> []
    | h::t -> (a*h)::(mul_int a t)

  let mul p q = 
    let rec mul_aux p q acc =
      match q with
      | [] -> []
      | h::t -> mul_aux p t (add (0::(mul_int h p)) acc) in
    mul_aux p q []
end