let rec repeat f n x = match n with
    | 0 -> x
    | k -> f (repeat f (k-1) x)

let product_left = List.fold_left ( *. ) 1.
let product_right lst = List.fold_right ( *. ) lst 1.


let clip n = 
if n < 0 then 0 
else if n > 10 then 10
else n

let cliplist lst = List.map clip lst

let rec cliplistrec = function
    | [] -> []
    | h::t -> (clip h)::(cliplistrec t)

let rec even n = if n = 0 then true else odd (n-1)
and odd n = if even (n-1) then false else true

let cube n = n * n * n

let (--) a b = 
    let rec aux a b acc =
        if b < a then acc
        else aux a (b-1) (b::acc)
    in aux a b []


let sum_cube_odd n = 0 -- n
    |> List.filter odd
    |> List.map cube
    |> List.fold_left (+) 0 



let rec exists_rec p = function
    | [] -> false
    | h::t -> p h || exists_rec p t

let exists_fold p lst =
    let g x y = x || p y
    in List.fold_left g false lst

let exists_lib p lst = List.map p lst |> List.fold_left (||) false


let rec budget_rec b = function
    | [] -> b
    | h::t -> budget_rec (b-h) t

let budget_foldleft b lst = b - List.fold_left (+) 0 lst
let budget_foldleft b lst = b - List.fold_right (+) lst 0


let uncurry f = function (x, y) -> f x y

let uncurried_listappend (lst1, lst2) = uncurry List.append (lst1, lst2)
let uncurried_charcompare = uncurry Char.compare
let uncurried_max (x, y)= uncurry max (x, y)


let curry f = function x -> (function y -> f (x, y))

let double_map f g lst = 
let h = function x -> f (g x)
in List.map h lst


let a = List.filter (fun x -> String.length x > 3)

let b = List.map ((+.) 1.)

let c strs sep = match strs with
    | [] -> ""
    | h::t -> List.fold_left (fun x -> (fun y -> x^sep^y)) h t

type 'a tree = 
| Leaf 
| Node of 'a * 'a tree * 'a tree

let rec treemap f = function
    | Leaf -> Leaf
    | Node (x, l, r) -> Node (f x, treemap f l, treemap f r)

let add1 = treemap ((+) 1)

let keys a = List.map fst a

let rec is_valid_matrix = function
    | [] -> false
    | x::[] -> List.length x > 0
    | x::y::t -> List.length x > 0 && List.length x = List.length y && is_valid_matrix (y::t)

let add_row_vectors x y = List.map2 (+) x y

let add_matrices a b = List.map2 add_row_vectors a b

let dot_product a b = List.fold_left (+) 0 (List.map2 ( * ) a b)

let rec dot_product_matrix a b = 
    match b with
    | h::t -> (dot_product a h)::(dot_product_matrix a t)
    | [] -> []


let transpose a =
    let rec add_row a b =
        match (a, b) with
        | (h1::t1, h2::t2) -> (h2@[h1])::(add_row t1 t2)
        | (h1::t1, []) -> [h1]::(add_row t1 [])
        | ([], []) -> []
        | (_, _) -> []
    in let rec transpose_aux a b =
        match a with
        | [] -> b
        | h::t -> transpose_aux t (add_row h b)
    in transpose_aux a []

let rec multiply_matrices a b =
    let rec aux a b = 
        match a with
        | h::t -> (dot_product_matrix h b)::(aux t b)
        | [] -> []
    in aux b (transpose a)