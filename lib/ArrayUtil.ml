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

(* Add all contents of Util and also export them *)

include Util

(*********************************************)
(*        Useful functions on arrays         *)
(*********************************************)

(* ----------  Week 02 functions  ---------- *)

(* Swapping two elements in an array *)

let swap arr i j = 
  let tmp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- tmp

(* Testing swap *)

let%test _ =
  let a = [|1; 2; 3; 4|] in
  swap a 0 1;
  a.(0) = 2
  
(* Printing a sub-array of elements *)

let print_int_sub_array l u arr =
  assert (l <= u);
  assert (u <= Array.length arr);
  Printf.printf "[| ";
  for i = l to u - 1 do
    Printf.printf "%d" arr.(i);
    if i < u - 1
    then Printf.printf "; "
    else ()      
  done;
  Printf.printf " |] "

(* Print the entire array *)

let print_int_array arr = 
  let len = Array.length arr in
  print_int_sub_array 0 len arr

(* Some examples in a nested module *)
module ArrayExamples = struct
  let a1 = [|6; 8; 5; 2; 3; 7; 0|]

  (* An array of 10 zeroes *)
  let a2 = Array.make 10 0

  let a3 = [|5 ;6; 1; 3; 3; 2; 2; 2; 15; 12|]
end 

(*******************************************************)
(* Auxiliary functions for checking if array is sorted *)
(*******************************************************)

let rec sorted ls = 
  match ls with 
  | [] -> true
  | h :: t -> 
    List.for_all (fun e -> e >= h) t && sorted t

(* Convert an array to list *)
(* The following three are an exercises *)

(* The following one is an exercise *)
(* subarray_to_list : int -> int -> 'a array -> 'a list *)
let subarray_to_list l u arr = 
  assert (l <= u);
  let res = ref [] in
  let i = ref (u - 1) in
  while l <= !i do
    res := arr.(!i) :: !res;
    i := !i - 1             
  done;
  !res

(* Exercise *)
let%test "subarray_to_list-1" =
  subarray_to_list 1 4 [|1; 2; 3; 4|] = [2; 3; 4]

(* Exercise *)
let%test "subarray_to_list-full-array" =
  subarray_to_list 0 4 [|1; 2; 3; 4|] = [1; 2; 3; 4]

(* Exercise *)
let%test "subarray_to_list-empty-array" =
  subarray_to_list 0 0 [||] = []

(* Exercise *)
let%test "array_to_lis-empty-range" =
  subarray_to_list 0 0 [|1; 2; 3|] = []

(* Converting the entire array to list *)

let array_to_list arr = 
  subarray_to_list 0 (Array.length arr) arr

(* Check if a sub-array [l...u) is sorted *)
  
let sub_array_sorted l u arr = 
  let ls = subarray_to_list l u arr in 
  sorted ls


let%test "sub_array_sorted-suffix" = 
  sub_array_sorted 2 4 [|5; 1; 2; 3|]

let%test "sub_array_sorted-prefix" = 
  sub_array_sorted 0 3 [|1; 2; 3; 5|]

let%test "sub_array_sorted-full" = 
  sub_array_sorted 0 4 [|1; 2; 3; 5|]

(* Sorting the entire array *)

let array_sorted arr = 
  sub_array_sorted 0 (Array.length  arr) arr


let%test "array_sorted" = 
  array_sorted [| 2; 3; 4; 5 |]


(* Ensuring that two lists have same elements *)
let same_elems ls1 ls2 =
  List.for_all (fun e ->
      List.find_all (fun e' -> e = e') ls2 =
      List.find_all (fun e' -> e = e') ls1
    ) ls1 &&
  List.for_all (fun e ->
      List.find_all (fun e' -> e = e') ls2 =
      List.find_all (fun e' -> e = e') ls1
    ) ls2

(* Check a minimum in a sub array *)
(* Uses Util.is_min *)

let is_min_sub_array l u arr min = 
  let ls = subarray_to_list l u arr in 
  is_min ls min

(* Generic testing procedure *)

let generic_array_sort_tester sorter a = 
  let a1 = Array.copy a in
  let ls1 = array_to_list a1 in
  sorter a1;
  let ls2 = array_to_list a1 in
  array_sorted a1 && same_elems ls1 ls2

(* ----------  Week 03 functions  ---------- *)

(*********************************************)
(*     Generating random elements            *)
(*********************************************)

(* An array of random integrs within a boundary `bound` *)
let generate_keys bound len = 
  let acc = ref [] in
  for _i = 0 to len - 1 do
    acc := (Random.int bound) :: ! acc
  done;
  !acc

(* A number `num` of words of a given length `length` *)
let generate_words length num =
  let random_ascii_char _ = 
    let rnd = (Random.int 26) + 97 in
    Char.chr rnd
  in
  let random_string _ = 
    let buf = Buffer.create length in
    for _i = 0 to length - 1 do
      Buffer.add_char buf (random_ascii_char ())
    done;
    Buffer.contents buf
  in
  let acc = ref [] in
  for _i = 0 to num - 1 do
    acc := (random_string ()) :: ! acc
  done;
  !acc

(* A list [0; ...; n] *)
let iota n = 
  let rec walk acc m = 
    if m < 0 
    then acc
    else walk (m :: acc) (m - 1)
  in
  walk [] n

let%test _ = iota 5 = [0; 1; 2; 3; 4; 5]

(* A better version of zipping two lists *)
let list_zip ls1 ls2 = 
  let rec walk xs1 xs2 k = match xs1, xs2 with
    | h1 :: t1, h2 :: t2 -> 
      walk t1 t2 (fun acc -> k ((h1, h2) :: acc))
    | _ -> k []
  in
  walk ls1 ls2 (fun x -> x)    

let list_to_array ls = match ls with
  | [] -> [||]
  | h :: t ->
    let arr = Array.make (List.length ls) h in
    List.iteri (fun i v -> arr.(i) <- v) ls;
    arr

let%test _ =
  list_to_array [1;2;3;4;5] = [|1;2;3;4;5|]

(*********************************************)
(*              Generating arrays            *)
(*********************************************)

(* Random array of integers *)
let generate_int_array len = 
  generate_keys len len |> list_to_array

(* Random array of strings *)
let generate_string_array len = 
  generate_words 5 len |> list_to_array
 
(* Random array of key-value pairs *)
let generate_key_value_array len = 
  let kvs = list_zip (generate_keys len len) (generate_words 5 len) in
  list_to_array kvs

(* ----------  Week 04 functions  ---------- *)

let print_kv_array arr lo hi = 
  let open Printf in 
  printf "[|";
  for i = lo to hi - 1 do
    printf "(%d, %s)" (fst arr.(i)) (snd arr.(i));
    if i < hi - 1 then printf "; "
  done;
  printf "|]"

let random_sorting_test_int sorter len = 
  let a = generate_int_array len in 
  generic_array_sort_tester sorter a

let random_sorting_test_string sorter len = 
  let a = generate_string_array len in 
  generic_array_sort_tester sorter a

let random_sorting_test_kv sorter len = 
  let a = generate_key_value_array len in 
  generic_array_sort_tester sorter a


(* ----------  Week 05 definitions  ---------- *)

(***********************************************)
(*          A functor for printing arrays      *)
(***********************************************)

module ArrayPrinter = functor (P : sig
    type t
    val pp : t -> string
  end) -> struct

    (* Printing machinery *)
    let print_sub_array l u arr =
      assert (l <= u);
      assert (u <= Array.length arr);
      Printf.printf "[| ";
      for i = l to u - 1 do
        Printf.printf "%s" (P.pp arr.(i));
        if i < u - 1
        then Printf.printf "; "
        else ()      
      done;
      Printf.printf " |] "
        
    let print_array arr = 
      let len = Array.length arr in
      print_sub_array 0 len arr              
  end

let to_list arr = array_to_list  arr

(***********************************************)
(*     Checking whether an array is sorted     *)
(***********************************************)

module SortChecker = functor (C : sig 
    type t 
    val comp : t -> t -> int 
  end) -> struct
  
  let rec sorted ls = 
    match ls with 
    | [] -> true
    | h :: t -> 
      List.for_all (fun e -> C.comp e h >= 0) t && sorted t
      
  let sub_array_sorted l u arr = 
    let ls = subarray_to_list l u arr in 
    sorted ls
      
  let array_sorted arr = 
    sub_array_sorted 0 (Array.length  arr) arr

  let sorted_spec arr1 arr2 = 
    array_sorted arr2 &&
    same_elems (to_list arr1) (to_list arr2)
      
end

(***********************************************)
(*            Comparing and Printing           *)
(***********************************************)

module type CompareAndPrint = sig
  (* Type of array elements *)
  type t
  (* For comparison *)
  val comp : t -> t -> int
  (* For pretty-printing *)
  val pp : t -> string
end

(*  Sorting via keys  *)
(* Exercise *)
let key_order_asc = 
  fun x y -> if
    fst x < fst y then -1
    else if fst x = fst y 
          then if snd x < snd y 
               then - 1
               else if snd x > snd y 
               then 1
               else 0
    else 1

