open Lekcija5


module Complex : ComplexSig = struct
    type t = float * float

    let zero = (0., 0.)
    let add (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
end

let fill_listqueue n =
    let rec loop n q =
        if n=0 then q
        else loop (n-1) (ListQueue.enqueue n q)
    in loop n ListQueue.empty

let fill_twolistqueue n =
    let rec loop n q =
        if n=0 then q
        else loop (n-1) (TwoListQueue.enqueue n q)
    in loop n TwoListQueue.empty


module BisDict : Dictionary = struct

    type ('k, 'v) t =
        | Leaf
        | Node of ('k * 'v) * ('k, 'v) t * ('k, 'v) t

    let empty = Leaf

    let rec insert k v = function
        | Leaf -> Node ((k, v), Leaf, Leaf)
        | Node ((k', v'), l, r) ->
            if k = k' then Node ((k, v), l, r)
            else if k > k' then Node ((k', v'), l, (insert k v r))
            else Node ((k', v'), (insert k v l), r)

    let rec lookup k = function
        | Leaf -> failwith ("Not_found")
        | Node ((k', v'), l, r) ->
            if k = k' then v'
            else if k > k' then lookup k r
            else lookup k l
end


module Fract : Fraction = struct

    type t = int * int

    let make n d =
        if d=0 then failwith ("d != 0 required")
        else (n, d)

    let numerator = function
        | (n, _) -> n

    let denominator = function
        | (_, d) -> d
    
    let to_string = function
        | (n, d) -> string_of_int n ^ "/" ^ string_of_int d

    let to_float = function
        | (n, d) -> (float_of_int n) /. (float_of_int d)

    let add (n1, d1) (n2, d2) = (n1*d2 + n2*d1, d1*d2)

    let mul (n1, d1) (n2, d2) = (n1*n2, d1*d2)
end

module Fractreduced : Fraction = struct

    type t = int * int

    let rec (%) a b =
        let res = a mod b
        in if res >= 0 then res else (res + b)

    let rec gcd a b = 
        if a < b then gcd b a
        else if a % b = 0 then b
        else gcd b (a % b)

    let rec reduce (n, d) =
        if d < 0 then reduce (-n, -d)
        else let g = gcd n d in (n / g, d / g)

    let make n d =
        if d=0 then failwith ("d != 0 required")
        else reduce (n, d)

    let numerator = function
        | (n, _) -> n

    let denominator = function
        | (_, d) -> d
    
    let to_string = function
        | (n, d) -> string_of_int n ^ "/" ^ string_of_int d

    let to_float = function
        | (n, d) -> (float_of_int n) /. (float_of_int d)

    let add (n1, d1) (n2, d2) = reduce (n1*d2 + n2*d1, d1*d2)

    let mul (n1, d1) (n2, d2) = reduce (n1*n2, d1*d2)
end

module CharMap = Map.Make(Char)

let dict = CharMap.(
    add 'A' "Alpha" empty |> add 'E' "Echo"
    |> add 'S' "Sierra" |> add 'V' "Victor" )

let dict = CharMap.remove 'A' dict

type date = {day : int; month : int}

module Date = struct
    type t = date

    let compare a b =
        let d = a.month - b.month in
        if d != 0 then d
        else a.day - b.day

    let to_string d = string_of_int d.day ^ "/" ^ string_of_int d.month
end

module DateMap = Map.Make(Date)

type calendar = string DateMap.t

let cal : calendar =
    DateMap.( add {day = 10; month = 10} "b-day" DateMap.empty
    |> add {day = 25; month = 12} "x-mas")

let print_calendar =
    let print_entry d s = print_endline (Date.to_string(d) ^ " : " ^ s) in
    DateMap.iter print_entry

let is_for d = CharMap.mapi (fun k v -> (Printf.sprintf "%c is for " k) ^ v) d

let first_after cal d =
    DateMap.(filter (fun k v -> Date.compare k d > 0) cal |> min_binding |> snd)

module Cis = struct
    type t = string
    let compare a b = String.(compare (uppercase_ascii a) (uppercase_ascii b))
end

module CisSet = Set.Make(Cis)

module type ToString = sig

    type t

    val to_string : t -> string

end

module Print (M : ToString) = struct

    include M

    let print t = M.to_string t

end

module Int = struct

    type t = int

    let to_string = string_of_int

end

module PrintInt = Print(Int)

module MyString = struct

    type t = string

    let to_string s = s

end 

module PrintString = Print(MyString)


module StringWithPrint = Print(struct

    include String

    let to_string s = s

end)