let lst1 = [1; 2; 3; 4; 5]
let lst2 = 1 :: 2 :: 3 :: 4 :: 5 :: []
let lst3 = [1] @ [2; 3; 4] @ [5]

let rec prod acc = function
    | [] -> acc
    | h :: t -> prod (acc * h) t

let product lst = prod 1 lst

let rec concat' str = function
    | [] -> str
    | h :: t -> concat' (str ^ h) t

let concat lst = concat' "" lst

let big_red = function
    | "bigred" :: _ -> true
    | _ -> false

let length_two_or_four = function
    | _ :: _ :: [] -> true
    | _ :: _ :: _ :: _ :: [] -> true
    | _ -> false

let first_two_equal = function
    | x :: y :: _ -> x = y
    | _ -> false

let fifth_element lst = if (List.length lst) < 5 then 0 else List.nth lst 4

let sort_desc lst = List.rev (List.sort Stdlib.compare lst)

let last_element lst = List.hd (List.rev lst)

let any_zeroes lst = (List.find_opt (fun x -> x = 0) lst) <> None

let rec take' c res n = function
    | [] -> res
    | h :: t -> if c = n then res else take' (c + 1) (h :: res) n t

let take n lst = List.rev (take' 0 [] n lst)

let rec drop' c n = function
    | [] -> []
    | h :: t -> if c = n then h :: t else drop' (c + 1) n t

let drop n lst = drop' 0 n lst

let rec rmv_monotonous inc = function
    | [] -> []
    | _ :: [] -> []
    | x :: y :: t -> if (if inc then x <= y else x >= y) then rmv_monotonous inc (y :: t) else (y :: t)


let is_unimodal lst = rmv_monotonous false (rmv_monotonous true lst) = []

let rec powerset_aux curr = function 
    | [] -> [curr]
    | h :: t -> powerset_aux curr t @ (powerset_aux (h :: curr) t)
    
let powerset set = powerset_aux [] set

let rec print_int_list = function
    | [] -> ()
    | h :: t -> print_endline (string_of_int h);
                print_int_list t

let print_int_list' lst = List.iter (fun x -> print_endline (string_of_int x)) lst


type student = { first_name : string ; last_name : string ; gpa : float }

let std = {first_name = "Pero"; last_name = "Peric"; gpa = 3.3}

let student_name std = (std.first_name, std.last_name)

let student_constructor name last_name gpa = {first_name = name; last_name = last_name; gpa = gpa}


type poketype = Normal | Fire | Water

type pokemon = {name : string; hp : int; ptype : poketype}

let charizard = {name = "charizard"; hp = 78; ptype = Fire}
let squirtle = {name = "squirtle"; hp = 44; ptype = Water}


let safe_hd = function
    | [] -> None
    | h :: t -> Some h

let safe_tl = function
    | [] -> None
    | h :: t -> Some (List.tl (h :: t))


let rec smallest_list_element_aux comp curr = function
    | [] -> curr
    | h :: t -> if comp curr h then smallest_list_element_aux comp curr t else smallest_list_element_aux comp h t

let smallest_list_element comp = function
    | [] -> None
    | h :: t -> Some (smallest_list_element_aux comp h t)


let max_hp lst = smallest_list_element (fun x y -> x.hp >= y.hp) lst


type date_like = int * int * int



let first t =
  let (x,_,_) = t in x

let second t =
  let (_,y,_) = t in y

let third t =
  let (_,_,z) = t in z

let is_before x y =
    if first x < first y then true
    else if first x > first y then false
    else if second x < second y then true
    else if second x > second y then false
    else if third x < third y then true
    else false


let earliest lst = smallest_list_element is_before lst

let insert k v d = (k, v) :: d

let rec lookup k = function
| [] -> None
| (k', v) :: t -> if k = k' then Some v else lookup k t


type suit = Hearts | Clubs | Diamonds | Spades

type rank = int

type card = rank * suit

let jack_hearts = (11, Hearts)

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign = if x > 0 then Pos else if x = 0 then Zero else Neg

let quadrant : int*int -> quad option = fun (x, y) ->
  match (sign x, sign y) with
    | (Pos, Pos) -> Some I
    | (Neg, Pos) -> Some II
    | (Neg, Neg) -> Some III
    | (Pos, Neg) -> Some IV
    | _ -> None

let quadrant_when : int*int -> quad option = function
    | (x, y) when sign x = Pos && sign y = Pos -> Some I
    | (x, y) when sign x = Neg && sign y = Pos -> Some II
    | (x, y) when sign x = Neg && sign y = Neg -> Some III
    | (x, y) when sign x = Pos && sign y = Neg -> Some IV
    | (_, _) -> None

type 'a tree = 
  | Leaf 
  | Node of 'a node
and 'a node = { 
  value: 'a; 
  left:  'a tree; 
  right: 'a tree
}

let rec depth = function
    | Leaf -> 0
    | Node n -> 1 + max (depth n.left) (depth n.right)


let rec shape x y =
    match (x, y) with
    | (Leaf, Leaf) -> true
    | (Node n1, Node n2) -> shape n1.left n2.left && shape n1.right n2.right
    | (Leaf, Node n) -> false
    | (Node n, Leaf) -> false


let rec tree_max = function
    | Leaf -> failwith "tree_max"
    | Node n -> begin
            let curr = match tree_max n.right with
                | exception (Failure s) -> n.value
                | x -> max n.value x in

            match tree_max n.left with
                | exception (Failure s) -> curr
                | x -> max x curr
        end

let rec list_max_aux curr = function
    | [] -> curr
    | h :: t -> list_max_aux (max h curr) t

let list_max = function
    | [] -> failwith "list_max"
    | h :: t -> list_max_aux h t

let list_max_string lst =
    match list_max lst with
        | exception (Failure s) -> "empty"
        | x -> string_of_int x

let sign' x = if x > 0 then `Pos else if x = 0 then `Zero else `Neg

let quadrant' = fun (x, y) ->
  match (sign' x, sign' y) with
    | (`Pos, `Pos) -> Some `I
    | (`Neg, `Pos) -> Some `II
    | (`Neg, `Neg) -> Some `III
    | (`Pos, `Neg) -> Some `IV
    | _ -> None



let rec min_max = function
    | Leaf -> `Empty
    | Node {value; left; right} -> begin
        let mini = match min_max left with
            | `Empty -> `Value value
            | `Not_bst -> `Not_bst
            | `Min_max (x, y) -> `Value x in
        let maxi = match min_max right with
            | `Empty -> `Value value
            | `Not_bst -> `Not_bst
            | `Min_max (x, y) -> `Value y in
        match (mini, maxi) with
            | (`Value x, `Value y) -> if(x > y) then `Not_bst else `Min_max (x, y)
            | _ -> `Not_bst
        end

let is_bst tree =
    match min_max tree with
    | `Empty -> true
    | `Not_bst -> false
    | `Min_max (x, y) -> true