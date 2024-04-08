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

(**********************************************************)
(*                 Searching in Strings                   *)
(**********************************************************)

(*********************)
(*   Naive search    *)
(*********************)

(*
Complexity: O(n * m)
*)

let very_naive_search text pattern = 
  let n = String.length text in
  let m = String.length pattern in
  if n < m then None
  else
    let k = ref 0 in
    let res = ref None in
    while !k <= n - m && !res = None do
      let candidate = String.sub text !k m in
      if candidate = pattern
      then res := Some !k
      else k := !k + 1
    done;
    !res


(***********************************)
(*   Testing  Very Naive search    *)
(***********************************)

let test_pattern_in search text pattern =
  let index = get_exn @@ search text pattern in
  let p' = String.sub text index (String.length pattern) in
  assert (pattern = p')

let test_pattern_not_in search text pattern =
  assert (search text pattern = None)

(*
Exercise: How would you suggest to test it?

* Construct one string
* Generate random string
* Test if in
* Test if not in
*)

let generate_string_and_patterns n m = 
  let ps_in = generate_words n m in
  let ps_not_in = 
    List.filter (fun pattern -> not (List.mem pattern ps_in)) @@
    generate_words n m in
  let text = String.concat "_" (List.rev ps_in) in
  (text, ps_in, ps_not_in)

module SearchExamples = struct
  
  let big = "abcdefghijklmnopeqrstuvabcsrtdsdqewgdcvaegbdweffwdajbjrag"
          
  let patterns = ["dsd"; "jrag"; "abc"]

end

let%test "Naive Search Works" = 
  let open SearchExamples in
  List.iter (fun p -> test_pattern_in very_naive_search big p) patterns;
  true

let%test "Naive Search True Positives" = 
  let (s, ps, _) = generate_string_and_patterns 500 5 in
  List.iter (fun p -> test_pattern_in very_naive_search s p) ps;
  true

let%test "Naive Search True Negatives" = 
  let (s, _, pn) = generate_string_and_patterns 500 5 in
  List.iter (fun p -> test_pattern_not_in very_naive_search s p) pn;
  true    

(*  

evaluate_on_random very_naive_search "Very Naive" 5000 20;;

*)    

(*  Slightly better result *)    

let naive_search text pattern = 
  let n = String.length text in
  let m = String.length pattern in
  if n < m then None
  else
    let k = ref 0 in
    let res = ref None in
    while !k <= n - m && !res = None do
      let j = ref 0 in
      while !j <= m - 1 && 
            text.[!k + !j] = pattern.[!j]
      do  j := !j + 1  done;
      if !j = m
      then res := Some !k
      else
        k := !k + 1
    done;
    !res

(*  

evaluate_on_random naive_search "Naive" 5000 20;;

*)    


(* Rewriting recursively *)

let naive_search_rec text pattern = 
  let n = String.length text in
  let m = String.length pattern in
  if n < m then None
  else
    let rec walk k =
      if k > n - m then None
      else (
      let j = ref 0 in
      while !j <= m - 1 && 
            text.[k + !j] = pattern.[!j]
      do  j := !j + 1  done;

      if !j = m
      then Some k
      else walk @@ k + 1)

    in walk 0
        

(*****************************)
(*   Testing Naive search    *)
(*****************************)

let%test "Naive Search Works" = 
  let open SearchExamples in
  List.iter (fun p -> test_pattern_in naive_search big p) patterns;
  true

let%test "Naive Search True Positives" = 
  let (s, ps, _) = generate_string_and_patterns 500 5 in
  List.iter (fun p -> test_pattern_in naive_search s p) ps;
  true

let%test "Naive Search True Negatives" = 
  let (s, _, pn) = generate_string_and_patterns 500 5 in
  List.iter (fun p -> test_pattern_not_in naive_search s p) pn;
  true

(*   Universal search tester   *)

let search_tester search = 
  let open SearchExamples in
  let (s, ps, pn) = generate_string_and_patterns 500 5 in
  List.iter (fun p -> test_pattern_in search big p) patterns;
  List.iter (fun p -> test_pattern_in search s p) ps;
  List.iter (fun p -> test_pattern_not_in search s p) pn;
  true

let%test _ = search_tester naive_search_rec

(*********************************************)
(*      Evaluating search algorithms         *)
(*********************************************)

let repetitive_string n = 
  let ast = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa" in
  let pat1 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab" in
  let pat2 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaac" in
  let mk n = 
    let t = List.init n (fun x -> if x = n - 1 then pat1 else ast) in
    String.concat "" t 
  in
  (mk n, [pat1], [pat2])


let evaluate_search search name s ps pn = 
  print_endline "";
  Printf.printf "[%s] Pattern in: " name;
  time (List.iter (fun p -> test_pattern_in search s p)) ps;
  Printf.printf "[%s] Pattern not in: " name;
  time (List.iter (fun p -> test_pattern_not_in search s p)) pn

let evaluate_on_random search name n m = 
  let (s, ps, pn) = generate_string_and_patterns n m in
  evaluate_search search name s ps pn 

let evaluate_on_repetitive search name n = 
  let (s, ps, pn) = repetitive_string n in
  evaluate_search search name s ps pn 
