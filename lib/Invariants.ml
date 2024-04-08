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

(**********************************************)
(*     Recursive Functions and Invariants     *)
(**********************************************)

(* A function that finds the smallest element in the list *)

let find_min ls = 
  let rec walk xs min = 
    match xs with
    | [] -> min
    | h :: t ->
      let min' = if h < min then h else min in
      walk t min'
  in match ls with
  | h :: t -> 
    let min = walk t h in
    Some min
  | _ -> None

(*  Let's start from some test *)

(* This is a precise specification of the algorithm,
   implemented by `find_min` *)

(* Some testing *)
let is_min ls m = 
  List.for_all (fun e -> e >= m) ls &&
  List.mem m ls
                  
let find_min_spec find_min_fun ls = 
  let result = find_min_fun ls in
  ls = [] && result = None ||
  is_min ls (get_exn result) 


let generic_test_find_min find_min = 
  find_min_spec find_min [] &&
  find_min_spec find_min [1; 2; 3] &&
  find_min_spec find_min [31; 42; 239; 5; 100]


let%test "find_min" = 
  generic_test_find_min find_min

(* Now let's write an invariant for `find_min`'s `walk` *)

(* Remember what an invariant is for:
  * It constrains the parameters of the function
  * It holds before every recursive  call the function 
  * It holds at the end of every function invocation
  * When we return from the function at the top level, 
    it should give us the desired correctness property, i.e., 
   `find_min_spec`
 *)

(* Unused variables are starting with _ *)

(* Postcondition *)
let find_min_walk_post ls _xs _min res = 
  is_min ls res

let find_min_walk_pre ls xs min = 
  is_suffix xs ls &&
  (is_min ls min ||
   List.exists (fun e -> e < min) xs)

(* Let us instrument walk_with invariant *) 

let find_min_with_invariant ls = 
  let rec walk xs min = 
    (* find_min_walk_pre ls xs min - holds here *)
    match xs with
    | [] -> 
      let res = min in
      assert (find_min_walk_post ls xs min res);
      res
    | h :: t ->
      let min' = if h < min then h else min in
      assert (find_min_walk_pre ls t min');
      let res = walk t min' in
      assert (find_min_walk_post ls xs min res);
      res

  in match ls with
  | h :: t -> 
    assert (find_min_walk_pre ls t h);
    let res = walk t h in
    assert (find_min_walk_post ls t h res);
    Some res
  | _ -> None

let%test "find_min with invariant" = 
  generic_test_find_min find_min_with_invariant
