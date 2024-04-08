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

include Heaps

module PriorityQueue(C: CompareAndPrint) = struct 

  (* Need to lift C to options *)
  module COpt = struct
    type t = C.t option
    
    let comp x y = match x, y with 
      | Some a, Some b -> C.comp a b
      | None, Some _ -> -1
      | Some _, None -> 1
      | None, None -> 0
        
    let pp x = match x with 
      | Some x -> C.pp x
      | None -> "None"
  end

  module H = Heaps(COpt)
  (* Do not inline, just include *)
  open H

  type heap = {
    heap_size : int ref;
    arr : H.t array
  }

  let mk_empty_queue size = 
    assert (size >= 0);
    {heap_size = ref 0;
     arr = Array.make size None}

  (* Make a priority queue from an array *)
  let mk_queue a = 
    let ls = List.map (fun e -> Some e) (to_list a) in
    let a' = list_to_array ls in
    build_max_heap a';
    {heap_size = ref (Array.length a);
     arr = a'}

  module P = ArrayPrinter(COpt)

  let print_heap h =     
    P.print_array h.arr

  (* Dereferencing the record *)
  let heap_maximum h = h.arr.(0)
             
  (* Fix this atrocity in the return type! *)
  let heap_extract_max h = 
    if !(h.heap_size) < 1 then None
    else
      let a = h.arr in
      (* Take the maximum element *)
      let max = a.(0) in
      (* Restore heap-ness *)
      a.(0) <- a.(!(h.heap_size) - 1);
      a.(!(h.heap_size) - 1) <- None;
      h.heap_size := !(h.heap_size) - 1;
      max_heapify !(h.heap_size) h.arr 0;
      (* Return the result *)
      max

  let heap_increase_key h i key =
    let a = h.arr in
    let c = comp key (a.(i)) >= 0 in
    if not c then (
      Printf.printf "A new key is smaller than current key!";
      assert false);
    a.(i) <- key;
    let j = ref i in
    while !j > 0 && comp (snd (H.parent a (!j))) a.(!j) < 0 do
      let pj = fst (H.parent a (!j)) in
      swap a !j pj;
      j := pj
    done

  let max_heap_insert h elem = 
    let hs = !(h.heap_size) in
    if hs >= Array.length h.arr 
    then raise (Failure "Maximal heap capacity reached!");
    h.heap_size := hs + 1;
    heap_increase_key h hs (Some elem)
end

(*********************************************)
(*         Testing priority queues           *)
(*********************************************)

module PQ = PriorityQueue(KV)
open PQ
open KVHeaps

let%test "PQ_heap" = 
  let q = mk_queue (generate_key_value_array 100) in
  let l = array_to_list q.arr in
  let ls = List.map (fun e -> get_exn e) l in
  let a = list_to_array ls in
  is_heap a

let%test "PQ_insert_not_none" = 
  let q = mk_queue (generate_key_value_array 100) in
  let m = heap_extract_max q in
  m <> None
      
 let%test "PQ_reinsert_max" = 
  let q = mk_queue (generate_key_value_array 100) in
  let m = heap_extract_max q in
  let e = get_exn m in 
  max_heap_insert q e;
  let m' = heap_extract_max q in
  m = m'
      
(*********************************************)
(*      Experimenting with Priority queue    *)
(*********************************************)

(*
let q = mk_queue (
 [|(6, "egkbs"); (4, "nugab"); (4, "xcwjg");
   (4, "oxfyr"); (4, "opdhq"); (0, "huiuv");
   (0, "sbcnl"); (2, "gzpyp"); (4, "hymnz");
   (2, "yxzro")|]);;
*)

