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

(*********************************************)
(*     Useful functions and data types       *)
(*********************************************)

(* ----------  Week 01 functions  ---------- *)

(* Raise an error with a message m *)

let error m = Failure m |> raise

(* Extract a value from an option *)

let get_exn o = match o with
  | Some e -> e
  | _ -> error "Empty option!"

(*  Auxiliary functions on lists *)

let rec remove_first ls n = 
  if n <= 0 then ls
  else match ls with 
    | [] -> []
    | h :: t -> remove_first t (n-1)
                  
let is_suffix xs ls = 
  let n1 = List.length xs in
  let n2 = List.length ls in
  let diff = n2 - n1 in
  if diff < 0 then false
  else
    let ls_tail = remove_first ls diff in
    ls_tail = xs

(* ----------  Week 02 functions  ---------- *)

let is_min ls min = 
  List.for_all (fun e -> min <= e) ls

let print_offset _ = 
  Printf.printf "  "

(* ----------  Week 03 functions  ---------- *)

(* Measuring execution time *)

let time f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Execution elapsed time: %f sec\n" (Sys.time () -. t);
  fx
