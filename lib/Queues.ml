(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2020 Ilya Sergey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

include ArrayUtil

(***********************************************)
(*             Representing queues             *)
(***********************************************)

(* An abstract specification for a queue *)

module type Queue = 
  sig
    type 'e t
    val mk_queue : int -> 'e t
    val is_empty : 'e t -> bool
    val is_full : 'e t -> bool
    val enqueue : 'e t -> 'e -> unit
    val dequeue : 'e t -> 'e option
    val queue_to_list : 'e t -> 'e list
  end

(* A bounded queue based on an array *)
module ArrayBasedQueue : Queue = struct

  (* A type of queue elements *)
  type 'e t = {
      elems : 'e option array;
      head : int ref;
      tail : int ref;
      size : int    
    }

  let mk_queue sz = {
      elems = Array.make sz None;
      head = ref 0;
      tail = ref 0;
      size = sz
    }
                  
  let is_empty q = 
    !(q.head) = !(q.tail) &&
      q.elems.(!(q.head)) = None
    
  let is_full q = 
    !(q.head) = !(q.tail) &&
      q.elems.(!(q.head)) <> None
    
  let enqueue q e = 
    if is_full q
    then error "The queue is full!"
    else begin
        let tl = !(q.tail) in
        q.elems.(tl) <- Some e;
        q.tail := 
          if tl = q.size - 1 
          then 0 
          else tl + 1
      end
    
  let dequeue q = 
    if is_empty q
    then None
    else begin
      let hd = !(q.head) in
      let res = q.elems.(hd) in
      q.elems.(hd) <- None; 
      q.head := 
        (if hd = q.size - 1 
         then 0 
         else hd + 1);
      res end
    
  let queue_to_list q = 
    let hd = !(q.head) in
    let tl = !(q.tail) in
    if is_empty q then [] 
    else if hd < tl then
      List.map get_exn (subarray_to_list hd (tl + 1) q.elems)
    else 
      let l1 = subarray_to_list hd q.size q.elems in
      let l2 = subarray_to_list 0 tl q.elems in
      List.map get_exn (l1 @ l2)

 end

(* A helper functor for printing a queue *)

module QueuePrinter(Q: Queue) = struct

  let print_queue q pp = 
    Printf.printf "[";
    List.iter (fun e ->
        Printf.printf "%s; " (pp e))
      (Q.queue_to_list q);
    Printf.printf "]\n"

end

module ABQPrinter = QueuePrinter(ArrayBasedQueue)

let pp (k, v) = Printf.sprintf "(%d, %s)" k v
              
let print_kv_queue q = ABQPrinter.print_queue q pp

(************************************)
(*      Testing Array-based queue   *)
(************************************)

let%test "testing a queue with an array" = 
  let open ArrayBasedQueue in
  let q = mk_queue 10 in
  let a = generate_key_value_array 10 in
  Array.iter (fun e -> enqueue q e) a;
  queue_to_list q = array_to_list a

(* TODO: add more tests for the queue to wrap around *)


(************************************)
(*        DLL-based queue           *)
(************************************)

open DoublyLinkedList

module DLLBasedQueue : Queue = struct
  open DoublyLinkedList
    
    type 'e t = {
      head : 'e dll_node option ref;
      tail : 'e dll_node option ref;
    }

    (* Tell about aliasing! *)
    let mk_queue _sz = 
      {head = ref None; 
       tail = ref None}
    
    let is_empty q = 
      !(q.head) = None
      
    let is_full _q = false
      
    let enqueue q e = 
      let n = mk_node e in
      (* Set the head *)
      (if !(q.head) = None
       then q.head := Some n);
      (* Extend the tail *)
      (match !(q.tail) with
       | Some t -> insert_after t n;
       | None -> ());
      q.tail := Some n 

    let dequeue q =
      match !(q.head) with
      | None -> None
      | Some n -> 
        let nxt = next n in
        q.head := nxt;
        remove n; (* This is not necessary *)
        Some (value n)

    let queue_to_list q = match !(q.head) with
      | None -> []
      | Some n -> to_list_from n

  end

(************************************)
(*      Testing Array-based queue   *)
(************************************)

let%test "testing a queue with an array" = 
  let open DLLBasedQueue in
  let q = mk_queue 10 in
  let a = generate_key_value_array 10 in
  Array.iter (fun e -> enqueue q e) a;
  queue_to_list q = array_to_list a

module DLQPrinter = QueuePrinter(DLLBasedQueue)
let print_queue q = DLQPrinter.print_queue q pp
