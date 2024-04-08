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

(*********************)
(* Hash-based search *)
(*********************)

(* Using the idea of hashing! *)
let hash_based_search text pattern = 
  let n = String.length text in
  let m = String.length pattern in
  if n < m then None
  else

    (* Hashing the pattern *)
    let hpattern = Hashtbl.hash pattern in 

    (* Hashing the string *)
    let i = ref 0 in
    let res = ref None in
    while !i <= n - m && !res = None do
      let rolling_hash = ref (Hashtbl.hash (String.sub text !i m)) in
      (if hpattern = !rolling_hash &&
          String.sub text !i m = pattern then
        res := Some !i);
      i := !i + 1
    done;
    !res

(* Testing *)    

include StringSearch    

let%test "Hash-Based Search Works" = 
  search_tester hash_based_search

let%test _ = 
  search_tester hash_based_search

let%test "Hash-Based Search True Positives - repetitive" = 
  let (s, ps, _) = repetitive_string 2000 in
  List.iter (fun p -> test_pattern_in hash_based_search s p) ps;
  true

let%test "Hash-Based Search True Negatives  - repetitive" = 
  let (s, _, pn) = repetitive_string 2000 in
  List.iter (fun p -> test_pattern_not_in hash_based_search s p) pn;
  true

(*

evaluate_on_repetitive naive_search "Naive" 5000;;

evaluate_on_repetitive hash_based_search "Hash search" 5000;;

evaluate_on_random hash_based_search "Hash search" 3000 20;;

evaluate_on_random naive_search "Naive" 3000 20;;

 *)  

(*********************)
(* Rabin-Karp search *)
(*********************)

let rk_hash text = 
  let h = ref 0 in
  for i = 0 to String.length text - 1 do
    h := !h + Char.code text.[i]
  done;
  !h

(*  Still O(m n) time *)

let rabin_karp_search text pattern = 
  let n = String.length text in
  let m = String.length pattern in
  if n < m then None
  else
    (* Compute as the sum of all characters in pattern *)
    let hpattern = rk_hash pattern in
    let rolling_hash = ref @@ rk_hash (String.sub text 0 m) in
    let i = ref 0 in
    let res = ref None in
    while !i <= n - m && !res = None do
      (if hpattern = !rolling_hash &&
          String.sub text !i m = pattern then
        res := Some !i);
         
      (* Update the hash *)
      (if !i <= n - m - 1
       then
         let c1 = Char.code text.[!i] in
         let c2 = Char.code text.[!i + m] in
         rolling_hash := !rolling_hash - c1 + c2);
      i := !i + 1
    done;
    !res


let rabin_karp_search_rec text pattern = 
  let n = String.length text in
  let m = String.length pattern in
  if n < m then None
  else
    (* Compute as the sum of all characters in pattern *)
    let hpattern = rk_hash pattern in
    
    let rec walk i rolling_hash =
      if i > n - m then None
      else if hpattern = rolling_hash &&
              String.sub text i m = pattern 
      then Some i
      else if i <= n - m - 1
      then 
        let c1 = Char.code text.[i] in
        let c2 = Char.code text.[i + m] in
        let rolling_hash' = rolling_hash - c1 + c2 in
        walk (i + 1) rolling_hash'
      else None
    in 
    walk 0 (rk_hash (String.sub text 0 m))


(*****************************)  
(* Testing Rabin-Karp search *)
(*****************************)


let%test "Rabin-Karp Search Works" = 
  search_tester rabin_karp_search

let%test _ = 
  search_tester rabin_karp_search_rec

let%test "Rabin-Karp Search True Positives - repetitive" = 
  let (s, ps, _) = repetitive_string 2000 in
  List.iter (fun p -> test_pattern_in rabin_karp_search s p) ps;
  true

let%test "Rabin-Karp Search True Negatives  - repetitive" = 
  let (s, _, pn) = repetitive_string 2000 in
  List.iter (fun p -> test_pattern_not_in rabin_karp_search s p) pn;
  true

(*

evaluate_on_repetitive naive_search "Naive" 5000;;

evaluate_on_repetitive rabin_karp_search "RK search" 5000;;

evaluate_on_random rabin_karp_search "RK search" 3000 20;;

evaluate_on_random naive_search "Naive" 3000 20;;

 *)  
