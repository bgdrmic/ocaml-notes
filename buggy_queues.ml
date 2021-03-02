(**********************************************************)
(*  WARNING:  The BuggyTwoListQueue module below          *)
(*            deliberately contains bugs!!!               *)
(**********************************************************)

module type Queue = sig
  (* An ['a t] is a queue whose elements have type ['a]. *)
  type 'a t

  (* The empty queue. *)
  val empty : 'a t

  (* Whether a queue is empty. *)
  val is_empty : 'a t -> bool

  (* [singleton x] is the queue containing just [x]. *)
  val singleton : 'a -> 'a t

  (* [enqueue x q] is the queue [q] with [x] added to the front. *)
  val enqueue : 'a -> 'a t -> 'a t

  (* [peek q] is [Some x], where [x] is the element at the front of the queue,
     or [None] if the queue is empty. *)
  val peek : 'a t -> 'a option

  (* [dequeue q] is [Some q'], where [q'] is the queue containing all the
     elements of [q] except the front of [q], or [None] if [q] is empty. *)
  val dequeue : 'a t -> 'a t option

  (* [to_list q] is [x1; x2; ...; xn], where [x1] is the element
     at the head of [q], ..., [xn] is the element at the end of [q]. *)
  val to_list : 'a t -> 'a list
end

module BuggyTwoListQueue : Queue = struct   
  (* [{front=[a;b]; back=[e;d;c]}] represents the queue
     containing the elements a,b,c,d,e. That is, the
     back of the queue is stored in reverse order.
     Representation invariant: [{front; back}] is in *normal form* if [front]
     being empty implies [back] is also empty.
     All queues passed into or out of the module
     must be in normal form. *)
  type 'a t = {front:'a list; back:'a list}

  let rep_ok q =
    match q.front, q.back with
    | ([], h::t) -> failwith("Queue is not in normal form.")
    | (_, _) -> q

  (* Helper function to ensure that a queue is in normal form. *)
  let norm q = 
    match rep_ok q with
    | {front=[]; back} -> rep_ok {front=List.rev back; back=[]}
    | q -> q

  let empty = {front=[]; back=[]}

  let is_empty q =
    match rep_ok q with
    | {front=[]; back=[]} -> true
    | _ -> false

  let singleton x = rep_ok (norm {front=[x]; back=[]})

  let enqueue x q = rep_ok (norm {q with back=x::q.back})

  let peek q =
    match rep_ok q with
    | {front=[]} -> None
    | {front=x::_} -> Some x

  let dequeue q = 
    match rep_ok q with
    | {front=[]; _} -> None
    | {front=_::xs; back} -> Some (rep_ok (norm {front=xs; back}))

  let to_list q =
    let {front;back} = rep_ok q
    in front @ List.rev back
end

