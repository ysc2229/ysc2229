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

open Extlib.IO

open Util
open BinaryEncodings

(***************************************************)
(*          Serialising/deserialising the tree     *)
(***************************************************)

type 'a tree = 
  | Node of 'a tree * 'a tree
  | Leaf of 'a

let tree1 = 
  let le = Leaf 'e' in
  let ld = Leaf 'd' in
  let la = Leaf 'a' in
  let lb = Leaf 'b' in
  let lc = Leaf 'c' in
  let lf = Leaf 'f' in
  Node (la,
        Node (Node (lc, lb), 
              Node (Node (lf, le), 
                    ld)))

let rec write_tree out t = 
  match t with 
  | Leaf c -> begin
      write_bits out ~nbits:1 1;
      write_bits out ~nbits:8 (int_of_char c)
    end
  | Node (l, r) ->
    write_bits out ~nbits:1 0;
    write_tree out l;
    write_tree out r
    
let rec read_tree input = 
  match read_bits input 1 with
  | 1 -> 
    let c = read_bits input 8 |> char_of_int in
    Leaf c
  | 0 ->
    let l = read_tree input in
    let r = read_tree input in
    Node (l, r)
  | _ -> raise (Failure "Cannot unparse tree!")
    

(* Test functions *)
let write_tree_to_binary = write_to_binary write_tree
let read_tree_from_binary = read_from_binary read_tree

let test_tree_serialization t = 
  let f = "tree.tmp" in
  write_tree_to_binary f t;
  let t' = read_tree_from_binary f in
  Sys.remove f;
  t = t'

(* Interim testing *)


let%test _ =
  test_tree_serialization tree1

(*******************************************)
(*    Computing frequency trees            *)
(*******************************************)

let cfreqs1 = [|('a', 45); ('b', 13); ('c', 12); 
                ('d', 16); ('e', 9); ('f', 5)|]

let make_tree_array freq_chars = 
  let n = Array.length freq_chars in
  let ftrees = Array.make n (Leaf 'a', 1) in
  for i = 0 to n - 1 do
    let (c, f) = freq_chars.(i) in
    ftrees.(i) <- (Leaf c, f)
  done;
  ftrees

(* Since we're using the max-queue, comparison is reversed *)
module CF = struct
  type t = char tree * int
  let comp x y = 
    if snd x < snd y then 1
    else if snd x > snd y then -1
    else 0
  let pp (_, f) = Printf.sprintf "[tree -> %d]" f
end

open PriorityQueue
module PQ = PriorityQueue(CF)
  
(* Taking an array freq_chars as an input *)
let compute_frequency_tree freq_chars = 
  let open PQ in
  let n = Array.length freq_chars in
  let ftrees = make_tree_array freq_chars in
  let q = mk_queue ftrees in
  for i = 0 to n - 2 do
    let (x, fx) = get_exn @@ heap_extract_max q in
    let (y, fy) = get_exn @@ heap_extract_max q in
    let n = (Node (x, y), fx + fy) in
    max_heap_insert q n
  done;
  fst @@ get_exn @@ heap_extract_max q

(* Interim testing *)

let%test _ =
  let t = compute_frequency_tree cfreqs1 in
  test_tree_serialization t

(*******************************************)
(*    Computing frequencies for a string   *)
(*******************************************)

let compute_freqs s = 
  let n = String.length s in
  let m = 256 in
  let freqs = Array.make m 0 in
  for i = 0 to n - 1 do
    let i = int_of_char s.[i] in
    freqs.(i) <- freqs.(i) + 1
  done;
  let cfreqs = Array.make m ('a', 0) in
  for i = 0 to m - 1 do
    cfreqs.(i) <- (char_of_int i, freqs.(i))
  done;
  cfreqs

(***********************************************)
(*   Create encoding table (lists of  bits)    *)
(***********************************************)

let build_table t = 
  let m = 256 in
  let table = Array.make m [] in 
  
  let rec make_codes t acc = 
    match t with
    | Leaf c -> 
      let i = int_of_char c in
      table.(i) <- acc
    | Node (l, r) -> begin
        make_codes l (acc @ [0]);
        make_codes r (acc @ [1])
      end
  in
  make_codes t [];
  table

(***********************************************)
(*       Serialize the compressed file         *)
(***********************************************)

(* Writing the encoding tree an characters *)
 let write_tree_and_data out (t, s) = 
   write_tree out t;
   let table = build_table t in
   let n = String.length s in 
   (* Write length *)
   write_bits out ~nbits:30 n;
   for i = 0 to n - 1 do
     let bits = table.(int_of_char s.[i])  in
     List.iter (fun bit ->
         write_bits out ~nbits:1 bit) bits
   done

let compress_string target s = 
  let freqs = compute_freqs s in
  let t = compute_frequency_tree freqs in 
  write_to_binary write_tree_and_data target (t, s)

let compress_file source target = 
  let s = read_file_to_single_string source in
  compress_string target s

(***********************************************)
(*         Read the compressed file            *)
(***********************************************)

let rec read_char_via_tree t input =
  match t with
  | Leaf c -> c
  | Node (l, r) ->
    let b = read_bits input 1 in 
    match b with
    | 0 -> read_char_via_tree l input
    | 1 -> read_char_via_tree r input
    | _ -> raise (Failure "This cannot happen!")

let read_encoded input = 
  let t = read_tree input in
  let n = read_bits input 30 in
  let buf = Buffer.create 100 in 
  for i = 0 to n - 1 do
    let ch = read_char_via_tree t input in
    Buffer.add_char buf ch
  done;
  Buffer.contents buf

let decompress_file filename = 
  read_from_binary read_encoded filename

(***********************************************)
(*         Read the compressed file            *)
(***********************************************)

let huffman_test s = 
  let filename = "archive.huf" in
  compress_string filename s;
  let s' = decompress_file filename in
  Sys.remove filename;
  s = s'

(***********************************************)
(*                    Tests                    *)
(***********************************************)

let%test "Huffman on War and Piece" = 
  let path = find_file "../../../resources/war-and-peace.txt" in
  let text = read_file_to_single_string path in
  huffman_test text

let%test "Huffman on Ulysses" = 
  let path = find_file "../../../resources/ulysses.txt" in
  let text = read_file_to_single_string path in
  huffman_test text
