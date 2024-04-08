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
(*         Selection Sort on Arrays          *)
(*********************************************)

(* The main procedure *)

let select_sort arr = 
  let len = A.length arr in
  for i = 0 to len - 1 do
    for j = i to len - 1 do
      if arr.(j) < arr.(i)
      then swap arr i j
      else ()
    done
  done

let%test "select_sort-1" = 
  generic_array_sort_tester select_sort ArrayExamples.a1

let%test "select_sort-2" = 
  generic_array_sort_tester select_sort ArrayExamples.a3

(* Tracing selection sort *)

let select_sort_print arr = 
  let len = A.length arr in
  for i = 0 to len - 1 do
    print_int_sub_array 0 i arr; 
    print_int_sub_array i len arr;
    print_newline ();

    for j = i to len - 1 do
      print_offset ();
      Printf.printf "j = %d, a[j] = %d, a[i] = %d: " j arr.(j) arr.(i);
      print_int_sub_array 0 i arr;
      print_int_sub_array i len arr;
      print_newline ();

      if arr.(j) < arr.(i)
      then swap arr i j
      else ()
    done;

    print_int_sub_array 0 (i + 1) arr; 
    print_int_sub_array (i + 1) len arr;
    print_newline (); print_newline ();
  done

(* Towards the invariant *)

let suffix_larger_than_prefix i arr =
  let len = A.length arr in
  let prefix = subarray_to_list 0 i arr in
  let suffix = subarray_to_list i len arr in
  List.for_all (fun e -> 
      List.for_all (fun f -> e <= f)  suffix
    ) prefix

let select_sort_outer_inv i arr =
  sub_array_sorted 0 i arr &&
  suffix_larger_than_prefix i arr

let select_sort_inner_inv j i arr = 
  is_min_sub_array i j arr arr.(i) &&
  sub_array_sorted 0 i arr &&
  suffix_larger_than_prefix i arr

(* Annotated with the invariant *)

let select_sort_inv arr = 
  let len = A.length arr in
  for i = 0 to len - 1 do
    assert (select_sort_outer_inv i arr);
    for j = i to len - 1 do
      assert (select_sort_inner_inv j i arr);
      if arr.(j) < arr.(i)
      then swap arr i j
      else ();
      assert (select_sort_inner_inv (j + 1) i arr);
    done;
    assert (select_sort_outer_inv (i + 1) arr);
  done

(* Testing the invariants *)

let%test "select_sort_inv-1" = 
  generic_array_sort_tester select_sort_inv ArrayExamples.a1

let%test "select_sort_inv-2" = 
  generic_array_sort_tester select_sort_inv ArrayExamples.a3

