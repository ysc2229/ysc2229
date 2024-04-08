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

(*********************************************)
(*      Inmplementation of Binary heaps      *)
(*********************************************)

(* Binary heaps as arrays *)
module Heaps (C : CompareAndPrint)  = struct
  include C
  include ArrayPrinter(C)


  (* 1. Main heap operations *)
  let parent arr i = 
    if i = 0 
    then (0, arr.(i)) 
    else 
      let j = (i + 1) / 2 - 1 in
      (j, arr.(j))

  let left arr i = 
    let len = Array.length arr in 
    let j = 2 * (i + 1) - 1 in
    if j < len 
    then Some (j, arr.(j))
    else None

  let right arr i = 
    let len = Array.length arr in 
    let j = 2 * (i + 1) in 
    if j < len 
    then Some (j, arr.(j))
    else None

  (* 2. Testing whether something is a heap *)
  let is_heap arr = 
    let len = Array.length arr - 1 in 
    let res = ref true in
    let i = ref 0 in
    while !i <= len / 2 - 1 && !res do
      let this = arr.(!i) in 
      let l = left arr !i in 
      let r = right arr !i in 
      (* Parent >= than left child *)
      let is_left = l = None || 
                    comp this (snd (get_exn l)) >= 0 in
      (* Parent >= than right child *)
      let is_right = l = None || 
                     comp this (snd (get_exn r)) >= 0 in
      res := !res && is_left && is_right;
      i := !i + 1
    done;
    !res

  (* The same with printing *)
  let is_heap_print ?(print = false) arr = 
    let open Printf in 
    let len = Array.length arr - 1 in 
    let res = ref true in
    let i = ref 0 in
    while !i <= len / 2 - 1 && !res do
      let this = arr.(!i) in 
      let l = left arr !i in 
      let r = right arr !i in 
      let is_left = l = None || 
                    comp this (snd (get_exn l)) >= 0 in
      let is_right = r = None || 
                     comp this (snd (get_exn r)) >= 0 in
      res := !res && is_left && is_right;
      (if (not !res && print) then (
         let (li, ll) = get_exn l in
         let (ri, rr) = get_exn r in
         printf "Out-of-order elements:\n";
         printf "Parent: (%d, %s)\n" !i (pp this);
         printf "Left: (%d, %s)\n" li (pp ll);
         printf "Right: (%d, %s)\n" ri (pp rr)
      ));
      i := !i + 1
    done;
    !res

  (* 3. Restoring the heap property for an element i *)
  let rec max_heapify heap_size arr i = 
    let len = Array.length arr in
    (* assert (heap_size <= Array.length arr); *)
    if i > (len - 1) / 2 || i >= heap_size then ()
    else
      let ai = arr.(i) in
      let largest = ref (i, arr.(i)) in
      let l = left arr i in 

      (* Shall we swap with the left child?.. *)
      if l <> None && 
         (fst (get_exn l)) < heap_size &&
         comp (snd (get_exn l)) (snd !largest) > 0 
      then largest := get_exn l;

      (* May be the right child is even bigger? *)
      let r = right arr i in 
      if r <> None && 
         (fst (get_exn r)) < heap_size &&
         comp (snd (get_exn r)) (snd !largest) > 0
      then largest := get_exn r;

      if !largest <> (i, ai) 
      (* Okay, there is a necessity to progress further... *)
      then begin 
          swap arr i (fst !largest); 
          max_heapify heap_size arr (fst !largest)
        end

  (* Question: Why does max_heapify terminate? *)

  (* 4: building a heap from an array *)
  let build_max_heap arr = 
    let len = Array.length arr in
    for i = (len - 1) / 2 downto 0 do
      max_heapify len arr i
    done

  (* Question: Why does the for-loop start only from 
               i = (len - 1) / 2, not from i = (len - 1)/? *)
        
  (* 5. Heapsort *)
  let heapsort arr = 
    let len = Array.length arr in
    let heap_size = ref len in
    build_max_heap arr;
    for i = len - 1 downto 1 do
      swap arr 0 i;
      heap_size := !heap_size - 1;
      max_heapify !heap_size arr 0;
    done
end

module KV = struct
  type t = int * string
  let comp = key_order_asc
  let pp (k, v) = Printf.sprintf "(%d, %s)" k v
end

(* Instantiate max-heaps *)
module KVHeaps = Heaps(KV)

module TestHeaps = struct
  let good_heap = 
    [|(16, "a");
      (14, "b");
      (10, "c");
      (8, "d");
      (7, "e");
      (9, "f");
      (3, "g");
      (2, "h");
      (4, "i");
      (1, "j");|]

  let bad_heap = 
    [|(16, "a");
      (14, "b");
      (9, "c");
      (8, "d");
      (7, "e");
      (11, "f");
      (3, "g");
      (2, "h");
      (4, "i");
      (1, "j");
      (1, "k");
      (10, "l");
      (6, "m");
|]
end

(*********************************************)
(*      Testing simple heap properties       *)
(*********************************************)

let%test _ = 
  KVHeaps.right TestHeaps.good_heap 0 = Some (2, (10, "c"))

let%test _ = 
  KVHeaps.left TestHeaps.good_heap 1 = Some (3, (8, "d"))

let%test _ = 
  KVHeaps.right TestHeaps.good_heap 1 = Some (4, (7, "e"))

let%test _ = 
  KVHeaps.left TestHeaps.good_heap 2 = Some (5, (9, "f"))

let%test _ = 
  KVHeaps.right TestHeaps.good_heap 2 = Some (6, (3, "g"))

let%test _ = 
  KVHeaps.parent TestHeaps.good_heap 9 = (4, (7, "e"))

let%test _ = 
  KVHeaps.parent TestHeaps.good_heap 4 = (1, (14, "b"))

let%test _ = 
  KVHeaps.parent TestHeaps.good_heap 1 = (0, (16, "a"))


(*********************************************)
(*     Testing that something is a heap       *)
(*********************************************)

let%test "Good Heap is a heap" = 
  let open KVHeaps in 
  is_heap TestHeaps.good_heap

let%test "Bad Heap is not a heap" = 
  not @@ KVHeaps.is_heap TestHeaps.bad_heap

(*********************************************)
(*     Testing max_heapify operation         *)
(*********************************************)

(* Exercise *)

let%test "Heapifying bad heap" = 
  let h = TestHeaps.bad_heap |> Array.copy in 
  let len = Array.length h in 
  KVHeaps.max_heapify len h 2;
  KVHeaps.is_heap h

(*********************************************)
(*        Testing build_max_heap             *)
(*********************************************)

(* Exercise *)

let%test "build_max_heap" = 
  let a = generate_key_value_array 100 in 
  KVHeaps.build_max_heap a;
  KVHeaps.is_heap a


(*********************************************)
(*           Testing heapsort                *)
(*********************************************)

(* Exercise *)

module KVChecker = SortChecker(KV)

let%test "heapsort" =
  let a = generate_key_value_array 100 in
  let b = Array.copy a in
  KVHeaps.heapsort a;
  KVChecker.sorted_spec b a
  
(* Define QuickSort on KV-pairs *)
module KVSorting = GeneralisedSorting.Sorting(KV)
let kv_sort_asc = KVSorting.sort

(* Exercise: testing two sorts *)
let%test _  =
  let a = generate_key_value_array 100 in
  let b = Array.copy a in
  kv_sort_asc a;
  KVHeaps.heapsort b;
  a = b

(* Exercise: timing quick-sort and heap-sort *)
