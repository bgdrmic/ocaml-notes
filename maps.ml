module type MyMap = sig

  (** [('k,'v) t] is the type of a map containing bindings 
      from keys of type ['k] to values of type ['v]. *)
  type ('k,'v) t
  
  (** [empty] is the map containing no bindings. *)
  val empty : ('k,'v) t
  
  (** [mem k m] is true if [k] is bound in [m] and false otherwise. *)
  val mem : 'k -> ('k,'v) t -> bool
  
  (** [find k m] is [v] iff [k] is bound to [v] in [m]. 
      Raises: [Not_found] if [k] is not bound in [m]. *)
  val find : 'k -> ('k,'v) t -> 'v
  
  (** [add k v m] is the map [m'] that contains the same bindings
      as [m], and additionally binds [k] to [v]. If [k] was
      already bound in [m], its old binding is replaced by
      the new binding in [m']. *)
  val add : 'k -> 'v -> ('k,'v) t -> ('k,'v) t
  
  (** [remove k m] is the map [m'] that contains the same bindings
      as [m], except that [k] is unbound in [m']. *)
  val remove : 'k -> ('k,'v) t -> ('k,'v) t

end 

(** Implementation of MyMap using association list which doesn't allow
    duplicate keys.
    Abstraction function: [(k1, v1), ... , (kn, vn)] represents
    bindings k1 -> v1, ... , kn -> vn
    Representation invariant: there are no two entries woth same key. *)
module MapNoDups = struct

  type ('k, 'v) t = ('k  * 'v) list

  let empty = []

  let mem = List.mem_assoc

  let find = List.assoc

  let remove = List.remove_assoc

  let add k v m = (k, v)::(remove k m)

end

(** Implementation of MyMap using association list which doesn't allow
    duplicate keys.
    Abstraction function: [(k1, v1), ... , (kn, vn)] represents
    bindings [a1]->[b1], ... , [am]->[bm] where [a1], ... , [am] are distinct
    and {a1, ... , am} = {k1, ... , kn}.
    if [ai] is equal to more than one key [k] from {k1, ... , kn},
    i.e. [ai] = [ki1] = ... = [kil] where i1, ... il are in asscending order,
    then [ai] -> [bi1] *)
module MapDups : MyMap = struct

  type ('k, 'v) t = ('k  * 'v) list

  let empty = []

  let mem = List.mem_assoc

  let find = List.assoc

  let rec remove k m =
    let removed = List.remove_assoc k m in
    if mem k removed then remove k removed
    else removed

  let add k v m = (k, v)::m

end

module MapFun : MyMap = struct

  type ('k, 'v ) t = 'k -> ('v option)

  let empty = fun x -> None

  let mem x m = 
    match m x with
    | None -> false
    | Some _ -> true

  let find x m =
    match m x with
    | None -> raise(Not_found)
    | Some y -> y

  let remove x m = fun t -> if t = x then None else m x

  let add k v m = fun t -> if t = k then Some v else m k

end