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

(**************************************************)
(*  A sub-module with arrays for testing search   *)
(**************************************************)
module ArraysForSearching = struct
 let a1 = 
   [|(9, "lgora"); (0, "hvrxd"); (2, "zeuvd"); (2, "powdp"); (8, "sitgt");
     (4, "khfnv"); (2, "omjkn"); (0, "txwyw"); (0, "wqwpu"); (0, "hwhju")|]

  let a2 = 
    [|(0, "vzxtx"); (1, "hjqxi"); (3, "wzgsx"); (4, "hkuiu"); (4, "bvyjr");
      (5, "hdgrv"); (5, "sobff"); (5, "bpelh"); (5, "xonjr"); (6, "qjzui");
      (6, "syhze"); (8, "xyzxu"); (9, "gaixr"); (10, "obght"); (11, "wmiwb");
      (11, "dzvmf"); (12, "teaum"); (13, "gazaf"); (14, "svemi"); (15, "rxpus");
      (15, "agajq"); (21, "vztoj"); (21, "oszgf"); (21, "ylxiy"); (23, "itosu");
      (26, "nondm"); (27, "yazoj"); (28, "nqzcl"); (29, "lfevj"); (31, "hfcds");
      (31, "pgrym"); (32, "yghgg")|]
end

(**************************************************)
(*  Linear search in an array of key-value pairs  *)
(**************************************************)

(* ('a * 'b) array -> 'a -> (int * ('a * 'b)) option *)
let linear_search arr k = 
  let len = Array.length arr in
  let res = ref None in
  let i = ref 0 in 
  while !i < len && !res = None do
    (if fst arr.(!i) = k 
    then res := Some ((!i, arr.(!i))));
    i := !i + 1
  done;
  !res

let%test "Testing linear search (positive)" = 
  (* Opening a module locally for testing *)
  let open ArraysForSearching in 
  linear_search a1 4 = Some (5, (4, "khfnv"))

let%test "Testing linear search (negative)" = 
  let open ArraysForSearching in 
  linear_search a1 10 = None


(**************************************************)
(*  Binary search in an array of key-value pairs  *)
(**************************************************)

(* ('a * 'b) array -> 'a -> ('a * 'b) option *)
let binary_search arr k = 
  let rec rank lo hi = 
    if hi <= lo 
    then 
      (* Empty array *)
      None
    (* Converged on a single element *)
    else 
      let mid = lo + (hi - lo) / 2 in
      if fst arr.(mid) = k 
      then Some (arr.(mid))
      else if fst arr.(mid) < k
      then rank (mid + 1) hi 
      else rank lo mid  
  in
  rank 0 (Array.length arr)

(* Testing binary search *)

let%test "Testing binary search (positive)" = 
  let open ArraysForSearching in 
  binary_search a2 8 = Some (8, "xyzxu")

let%test "Testing binary search (negative)" = 
  let open ArraysForSearching in 
  binary_search a2 25 = None

(*****************************************)
(*        Tracing binary search          *)
(*****************************************)

let binary_search_print arr k = 
  let rec rank lo hi = 
    Printf.printf "Subarray: [";
    let ls = subarray_to_list lo hi arr in
    List.iter (fun (k, v) -> Printf.printf "(%d, %s); " k v) ls;
    Printf.printf "]\n\n";
    if hi <= lo 
    then 
      (* Empty array *)
      None
    (* Converged on a single element *)
    else 
      let mid = lo + (hi - lo) / 2 in
      if fst arr.(mid) = k 
      then Some (arr.(mid))
      else if fst arr.(mid) < k
      then rank (mid + 1) hi 
      else rank lo mid  
  in
  rank 0 (Array.length arr)

(*****************************************)
(*         Why does the search work?     *)
(*****************************************)

(* What is the precondition? *)

let binary_search_rank_pre arr lo hi k = 
  let len = Array.length arr in 
  let ls = subarray_to_list 0 len arr in
  let ls' = subarray_to_list lo hi arr in
  if List.exists (fun e -> fst e = k) ls
  then List.exists (fun e -> fst e = k) ls'
  else not (List.exists (fun e -> fst e = k) ls')

(* Annotating with an invariant *)

let binary_search_inv arr k = 
  let rec rank lo hi = 
    Printf.printf "lo = %d, hi = %d\n" lo hi;
    Printf.printf "Subarray: [";
    let ls = subarray_to_list lo hi arr in
    List.iter (fun (k, v) -> Printf.printf "(%d, %s); " k v) ls;
    Printf.printf "]\n";
    if hi <= lo 
    then 
      (* Empty array *)
      None
      (* Converged on a single element *)
    else 
      let mid = lo + (hi - lo) / 2 in
      Printf.printf "mid = %d\n" mid;
      if fst arr.(mid) = k 
      then Some (arr.(mid))
      else if fst arr.(mid) < k
      then
        (Printf.printf "THEN: lo = %d, hi = %d\n\n" (mid + 1) hi;
        assert (binary_search_rank_pre arr (mid + 1) hi k);
        rank (mid + 1) hi) 
      else 
        (Printf.printf "ELSE: lo = %d, hi = %d\n\n" lo mid;
        assert (binary_search_rank_pre arr lo mid k);
         rank lo mid)
  in
  let len = Array.length arr in 
  assert (binary_search_rank_pre arr 0 len k);
  rank 0 len

(* Testing *)

(* let%test _ = 
 *   let open ArraysForSearching in 
 *   binary_search_inv a2 8 = Some (8, "xyzxu")
 * 
 * let%test _ = 
 *   let open ArraysForSearching in 
 *   binary_search_inv a2 25 = None *)
