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


(*************************)
(* Comparing performance *)
(*************************)

open Util
include StringSearch
include RabinKarp
include KMP

(*****************************************************)
(*                  Random strings                   *)
(*****************************************************)

(* compare_string_search 20000 50;; *)

let compare_string_search n m =
  let (s, ps, pn) = generate_string_and_patterns n m in
  evaluate_search naive_search "Naive" s ps pn;
  evaluate_search rabin_karp_search "Rabin-Karp" s ps pn;
  evaluate_search search_kmp "Knuth-Morris-Pratt"  s ps pn

(*****************************************************)
(*                  Repetitive strings               *)
(*****************************************************)


(* Try on compare_string_search_repetitive 50000;; *)

let compare_string_search_repetitive n =
  let (s, ps, pn) = repetitive_string n in
  evaluate_search naive_search  "Naive"  s ps pn;
  evaluate_search rabin_karp_search "Rabin-Karp"  s ps pn;
  evaluate_search search_kmp "Knuth-Morris-Pratt"  s ps pn
