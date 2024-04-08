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

(*************************************)
(*         N-queens problem          *)
(*************************************)

let is_safe board row col = 
  let n = Array.length board in

  (* Check this row on the left *)
  let rec check_row_left i = 
    if i < 0 then true
    else if board.(row).(i) = 1 then false
    else check_row_left (i - 1)
  in

  let rec check_left_up_diag i j = 
    if i < 0 || j < 0 then true
    else if board.(i).(j) = 1 then false
    else check_left_up_diag (i - 1) (j - 1)
  in

  let rec check_left_down_diag i j = 
    if i >= n || j < 0 then true
    else if board.(i).(j) = 1 then false
    else check_left_down_diag (i + 1) (j - 1)

  in

  check_row_left col &&
  check_left_up_diag row col &&
  check_left_down_diag row col

(***********************************************)
(*      Recursive procedure with backtracking  *)    
(***********************************************)

let rec solver board n col = 
  let rec loop i = 
    if i = n then false
    else if is_safe board i col
    then begin
        board.(i).(col) <- 1;
        if solver board n (col + 1)
        then true
        (* Backtracking *)
        else begin
            board.(i).(col) <- 0;
            loop (i + 1)
          end
      end
    else loop (i + 1)
  in
  if col >= n 
  then true
  else loop 0

let solve_n_queens board = 
  let n = Array.length board in
  let _ = solver board n 0 in
  board

(* Question: What is the complexity? *)

(***********************************************)
(*             Checking the result             *)    
(***********************************************)

let mk_board n = 
  let board = Array.make n (Array.make n 0) in
  for i = 0 to n - 1 do
    board.(i) <- Array.make n 0
  done;
  board

let print_board board = 
  let n = Array.length board in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      Printf.printf "%d  " board.(i).(j);
    done;
    print_endline ""
  done

