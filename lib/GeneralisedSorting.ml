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
(*   Sorting with arbitrary comparator   *)
(*****************************************)
let generic_quick_sort arr ~comp = 
  let partition arr lo hi = 
    if hi <= lo then lo
    else
      let pivot = arr.(hi - 1) in
      let i = ref lo in 
      for j = lo to hi - 2 do
        if comp arr.(j) pivot <= 0 
        then begin
          swap arr !i j;
          i := !i + 1
        end
      done;
      swap arr !i (hi - 1);
    !i
  in
  let rec sort arr lo hi = 
    if hi - lo <= 1 then ()
    else 
      let mid = partition arr lo hi in
      sort arr lo mid;
      sort arr mid hi
  in
  sort arr 0 (Array.length arr)

let int_order_asc = 
  fun x y -> if
    x < y then -1
    else if x = y then 0
    else 1
      
(* Testing for integers *)
let%test _ =
  random_sorting_test_int 
    (generic_quick_sort ~comp:int_order_asc) 500

(*****************************************)
(*          Sorting via functor          *)
(*****************************************)

module type Comparable = sig
  type t
  val comp : t -> t -> int
end

module Sorting(Comp: Comparable) = struct
  include Comp

  let sort arr  = 
    let partition arr lo hi = 
      if hi <= lo then lo
      else
        let pivot = arr.(hi - 1) in
        let i = ref lo in 
        for j = lo to hi - 2 do
          if comp arr.(j) pivot <= 0 
        then begin 
          swap arr !i j;
          i := !i + 1
        end
      done;
      swap arr !i (hi - 1);
    !i
  in
  let rec sort_aux arr lo hi = 
    if hi - lo <= 1 then ()
    else 
      let mid = partition arr lo hi in
      sort_aux arr lo mid;
      sort_aux arr mid hi
  in
  sort_aux arr 0 (Array.length arr)
end


module IntAsc = struct
  type t = int
  let comp = int_order_asc
end

module AscIntSorting = Sorting(IntAsc)
let int_sort_asc = AscIntSorting.sort

let%test _ = random_sorting_test_int int_sort_asc 500

