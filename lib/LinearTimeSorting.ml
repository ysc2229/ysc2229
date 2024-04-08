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

(*****************************************)
(*          Linear-time sorting          *)
(*****************************************)

(*****************************************)
(*          Bounded bucket sort          *)
(*****************************************)

let simple_bucket_sort bnum arr = 
  let buckets = Array.make bnum [] in
  let len = Array.length arr in 
  for i = 0 to len - 1 do
    let key = arr.(i) in
    let bindex = key mod bnum in
    let b = buckets.(bindex) in
    buckets.(bindex) <- arr.(i) :: b
  done;
  let res = ref [] in
  for i = bnum - 1 downto 0 do
    res := List.append (List.rev (buckets.(i))) !res
  done;
  list_to_array !res

(* Testing bucket sort *)

let%test "simple-bucket-sort" = 
  let a = generate_int_array 500 in 
  let b = simple_bucket_sort 500 a in
  array_sorted b && 
  same_elems (array_to_list a) (array_to_list b)

(*****************************************)
(*          Proper bucket sort           *)
(*****************************************)

(* Proper bucket sort *)
let bucket_sort max ?(bnum = 1000) arr = 
  let buckets = Array.make bnum [] in
  let len = Array.length arr in 
  for i = 0 to len - 1 do
    let key = arr.(i) in
    let bind = key * bnum / max in
    let b = buckets.(bind) in
    buckets.(bind) <- arr.(i) :: b
  done;
  let res = ref [] in
  for i = bnum - 1 downto 0 do
    let bucket_contents = List.rev (buckets.(i)) in 
    let sorted_bucket = InsertSort.insert_sort bucket_contents in
    res := List.append sorted_bucket !res
  done;
  list_to_array !res

(* Testing *)
  
let%test "proper-bucket-sort" = 
  let a = generate_int_array 1000 in 
  let b = bucket_sort 1000 a in
  array_sorted b && 
  same_elems (array_to_list a) (array_to_list b)


(*****************************************)
(*               Radix sort              *)
(*****************************************)

(* Sort an array of pairs by keys *)

let kv_bucket_sort bnum arr = 
  let buckets = Array.make bnum [] in
  let len = Array.length arr in 
  for i = 0 to len - 1 do
    let key = fst arr.(i) in
    let bindex = key mod bnum in
    let b = buckets.(bindex) in
    buckets.(bindex) <- arr.(i) :: b
  done;
  let res = ref [] in
  for i = bnum - 1 downto 0 do
    res := List.append (List.rev (buckets.(i))) !res
  done;
  list_to_array !res

(* Radix sort *)
let radix_sort arr = 
  let len = Array.length arr in
  let max_key = 
    let res = ref 0 in
    for i = 0 to len - 1 do
      if arr.(i) > !res 
      then res := arr.(i)
    done; !res
  in
  if len = 0 then arr
  else
    let radix = ref max_key in
    let ls = array_to_list arr in
    let combined = list_to_array (list_zip ls ls) in
    let res = ref combined in
    while !radix > 0 do
      res := kv_bucket_sort 10 !res;
      for i = 0 to len - 1 do
        let (k, v) = !res.(i) in
        !res.(i) <- (k / 10, v)
      done;
      radix := !radix / 10
    done;
    let result_list = array_to_list !res in
    list_to_array @@ List.map snd result_list


let%test "radix-sort" = 
  let a = generate_int_array 1000 in 
  let b = radix_sort a in
  array_sorted b && 
  same_elems (array_to_list a) (array_to_list b)
