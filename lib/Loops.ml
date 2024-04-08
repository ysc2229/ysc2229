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

open Util
open Invariants

(***********************************************)
(*              Going imperative               *)
(***********************************************)

let find_min_loop ls = 
  
  let loop cur_tail cur_min = 
    while !cur_tail <> [] do
      let xs = !cur_tail in
      let h = List.hd xs in
      let min = !cur_min in
      cur_min := if h < min then h else min;
      cur_tail := List.tl xs
    done;
    !cur_min

  in match ls with
  | h :: t -> 
    let cur_tail = ref t in
    let cur_min = ref h in
    let min = loop cur_tail cur_min in
    Some min
  | _ -> None

let%test "find_min_loop" = 
  generic_test_find_min find_min_loop

(*  Now we need to assign the loop invariant  *)

let find_min_loop_inv ls = 
  
  let loop cur_tail cur_min = 
    assert (find_min_walk_pre ls !cur_tail !cur_min);
    while !cur_tail <> [] do
      let xs = !cur_tail in
      let h = List.hd xs in
      let min = !cur_min in
      cur_min := if h < min then h else min;
      cur_tail := List.tl xs;
      assert (find_min_walk_pre ls !cur_tail !cur_min);
    done;
    !cur_min

  in match ls with
  | h :: t -> 
    let cur_tail = ref t in
    let cur_min = ref h in
    assert (find_min_walk_pre ls !cur_tail !cur_min);
    let min = loop cur_tail cur_min in
    assert (find_min_walk_post ls !cur_tail !cur_min min);
    Some min
  | _ -> None

let%test "find_min_loop with invariant" = 
  generic_test_find_min find_min_loop_inv
