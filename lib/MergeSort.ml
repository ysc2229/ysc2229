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
(*  Merging two arrays into a third one  *)
(*****************************************)

let merge from1 from2 dest lo hi =
  let len1 = Array.length from1 in 
  let len2 = Array.length from2 in 
  let i = ref 0 in
  let j = ref 0 in
  for k = lo to hi - 1 do
    if !i >= len1 
    (* from1 is exhausted, copy everythin from from2 *)   
    then (dest.(k) <- from2.(!j); j := !j + 1)
    else if !j >= len2
    (* from2 is exhausted, copy everythin from from1 *)   
    then (dest.(k) <- from1.(!i); i := !i + 1)
    else if from1.(!i) <= from2.(!j)
    (* from1 has a smaller element, copy it, advance its index *)
    then (dest.(k) <- from1.(!i); i := !i + 1)
    (* from2 has a smaller element, copy it, advance its index *)
    else (dest.(k) <- from2.(!j); j := !j + 1)
  done

(****************************************)
(*             Merge-sort               *)
(****************************************)

let copy_array arr lo hi =
  let len = hi - lo in
  assert (len >= 0);
  if len = 0 then [||]
  else 
    let res = Array.make len arr.(lo) in
    for i = 0 to len - 1 do
      res.(i) <- arr.(i + lo)
    done;
    res
    
let merge_sort arr = 
  let rec sort a = 
    let lo = 0 in
    let hi = Array.length a in
    if hi - lo <= 1 then ()
    else
      let mid = lo + (hi - lo) / 2 in
      let from1 = copy_array a lo mid in
      let from2 = copy_array a mid hi in
      sort from1; 
      sort from2;
      merge from1 from2 a lo hi
  in
  sort arr

let%test "Merge-sort is correct for ints" =
  let a = generate_int_array 10 in
  generic_array_sort_tester merge_sort a

let%test "Merge-sort is correct for strings" =
  let a = generate_string_array 10 in
  generic_array_sort_tester merge_sort a

let%test "Merge-sort is correct for pairs" =
  let a = generate_key_value_array 10 in
  generic_array_sort_tester merge_sort a

(****************************************)
(*        Merge-sort invariant         *)
(****************************************)

(* Pre/postconditions of merge *)

let merge_pre from1 from2 = 
  array_sorted from1 && array_sorted from2

let merge_post from1 from2 arr lo hi = 
  array_sorted arr &&
  (let l1 = array_to_list from1 in
  let l2 = array_to_list from2 in
  let l = subarray_to_list lo hi arr in
  same_elems (l1 @ l2) l)

(* Annotated merge sort *)  

let merge_sort_inv arr = 
  let rec sort a = 
    let hi = Array.length a in
    let lo = 0 in
    if hi - lo <= 1 then ()
    else
      let mid = lo + (hi - lo) / 2 in
      let from1 = copy_array a lo mid in
      let from2 = copy_array a mid hi in
      sort from1; sort from2;
      assert (merge_pre from1 from2);
      merge from1 from2 a lo hi;
      assert (merge_post from1 from2 a lo hi)
  in
  sort arr

(* Testing the invariant-annotated merge sort *)

let%test _ =
  generate_int_array 10 |>
  generic_array_sort_tester merge_sort

let%test _ =
  generate_string_array 10 |>
  generic_array_sort_tester merge_sort

let%test _ =
  generate_key_value_array 10 |>
  generic_array_sort_tester merge_sort
