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

open ArrayUtil
module A = Array

(*********************************************)
(*         Insertion Sort on Arrays          *)
(*********************************************)

(* Insert_sort: loop-based implementation *)
let insert_sort arr = 
  let len = A.length arr in
  for i = 0 to len - 1 do
    let j = ref i in 
    while !j > 0 && arr.(!j) < arr.(!j - 1) do
      swap arr !j (!j - 1);
      j := !j - 1
    done
  done

(* Testing InsertSort *)

let%test "insert-sort-sorts" = 
  let a =  ArrayExamples.a3 in
  let ls1 = array_to_list a in
  insert_sort a;
  let ls2 = array_to_list a in
  array_sorted a && 
  same_elems ls1 ls2

(* TODO: generalise this procedure. 
   See ArrayUtil.generic_array_sort_tester *)

(* Reconstructing the invariant by tracing *)

let insert_sort_print arr = 
  let len = A.length arr in
  for i = 0 to len - 1 do
    print_int_sub_array 0 i arr; 
    print_int_sub_array i len arr;
    print_newline ();
    let j = ref i in 
    while !j > 0 && arr.(!j) < arr.(!j - 1) do
      print_offset ();
      print_int_sub_array 0 (i + 1) arr;
      print_int_sub_array (i + 1) len arr;
      print_newline ();
      swap arr !j (!j - 1);
      j := !j - 1;
    done;
    print_int_sub_array 0 (i + 1) arr; 
    print_int_sub_array (i + 1) len arr; 
    print_newline (); print_newline ()
  done

(* The invariant *)

let insert_sort_inner_loop_inv j i arr = 
  is_min_sub_array !j i arr arr.(!j) &&
  sub_array_sorted 0 !j arr && 
  sub_array_sorted (!j + 1) (i + 1) arr

let insert_sort_outer_loop_inv i arr = 
  sub_array_sorted 0 i arr

(* Checking the invariant *)

let insert_sort_inv arr = 
  let len = A.length arr in
  for i = 0 to len - 1 do
    assert (insert_sort_outer_loop_inv i arr);    
    let j = ref i in 
    while !j > 0 && arr.(!j) < arr.(!j - 1) do
      assert (insert_sort_inner_loop_inv j i arr);
      swap arr !j (!j - 1);
      j := !j - 1;
      assert (insert_sort_inner_loop_inv j i arr);
    done;
    assert (insert_sort_outer_loop_inv (i + 1) arr)
  done

let%test "insert_sort_inv" = 
  generic_array_sort_tester insert_sort_inv ArrayExamples.a3
