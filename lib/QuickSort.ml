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
(*              Quick Sort               *)
(*****************************************)

(* Quick-sort for lists *)
let rec qsort ls = 
  match ls with 
  | [] -> ls 
  | [z] -> ls 
  | h :: t ->
    let l1 = List.filter (fun e -> e <= h) t in
    let l2 = List.filter (fun e -> e > h) t in
    (qsort l1) @ [h] @ (qsort l2)

let%test _ =
  let ls = generate_int_array 50 |> array_to_list in
  let ls' = qsort ls in
  sorted ls' && same_elems ls ls'


(* Partitioning a sub-array array *)

let partition arr lo hi = 
  if hi <= lo then lo
  else
    let pivot = arr.(hi - 1) in
    let i = ref lo in 
    for j = lo to hi - 2 do
      if arr.(j) <= pivot 
      then begin
        swap arr !i j;
        i := !i + 1
      end
    done;
    swap arr !i (hi - 1);
    !i

(* Invariant: everything left of i is <= than pivot *)

let%test _ = 
  let a = [|4;5;1;2;3|] in
  let res = partition a 0 5 in
  a = [|1; 2; 3; 5; 4|] && res = 2

(* Paritition of int array with tracing *)

let partition_print arr lo hi = 
  let open Printf in
  if hi <= lo then lo
  else
    let pivot = arr.(hi - 1) in
    printf "pivot = %d\n" pivot;
    let i = ref lo in 
    for j = lo to hi - 2 do

      printf "lo = %d to  i = %d: " lo !i;
      print_int_sub_array lo !i arr; print_newline ();
      printf "i = %d  to j = %d: " !i j;
      print_int_sub_array !i j arr; print_newline ();
      printf "j = %d  to hi = %d: " j hi;
      print_int_sub_array j (hi - 1) arr; print_newline ();
      print_newline ();

      if arr.(j) <= pivot 
      then begin
        swap arr !i j;
        i := !i + 1
      end
    done;
    swap arr !i (hi - 1);
    print_int_sub_array lo hi arr; print_newline ();
    !i

(*****************************************)
(*                Sorting                *)
(*****************************************)

(* Invariants: all elements are on the right sides (less/ greater than) *)

let quick_sort arr = 
  let rec sort arr lo hi = 
    if hi - lo <= 1 then ()
    else begin
      let mid = partition arr lo hi in
      sort arr lo mid;
      sort arr (mid + 1) hi
    end
  in
  sort arr 0 (Array.length arr)

(* Testing quick sort *)

let%test "qsort-int" = 
  random_sorting_test_int quick_sort 500

let%test "qsort-string" = 
  random_sorting_test_string quick_sort 500

let%test "qsort-string" = 
  random_sorting_test_kv quick_sort 500

(* Tracing on int array*)
let quick_sort_print arr = 
  let open Printf in
  let rec sort arr lo hi = 
    if hi - lo <= 1 then ()
    else 
      let mid = partition arr lo hi in
      printf "lo = %d, hi = %d\n" lo hi;
      print_int_sub_array lo hi arr; print_newline ();
      printf "mid = %d\n" mid; print_newline ();
      sort arr lo mid;
      sort arr (mid + 1) hi
  in
  sort arr 0 (Array.length arr)
