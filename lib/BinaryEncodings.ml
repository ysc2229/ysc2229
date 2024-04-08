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

include ReadingFiles

open Extlib.IO

(************************************)
(*         Working with bits        *)
(************************************)

let read_from_binary deserialize filename =  
  Core.In_channel.with_file ~binary:true filename 
    ~f:(fun file_input ->
        let bits_input = input_bits @@ input_channel file_input in
        deserialize bits_input)


let write_to_binary serialize filename data = 
  Core.Out_channel.with_file filename ~append:false ~binary:true ~f:(fun file_out ->
      let bits_output = output_bits @@ output_channel file_out ~cleanup:true in
      serialize bits_output data;
      (* Padding from the end -- important! *)
      flush_bits bits_output)

(************************************)
(*      Writing encoded strings     *)
(************************************)

let write_string_to_binary filename text = 
  let serialize out text = 
    let size = String.length text in
    for i = 0 to size - 1 do
      let ch = int_of_char text.[i] in      
      write_bits out ~nbits:8 ch;
    done
  in
  write_to_binary serialize filename text
    
let read_string_from_binary filename =  
  let deserialize input = 
    let buffer = Buffer.create 1 in
    (try
       while true do
         let bits = read_bits input 8 in
         let ch = char_of_int bits in   
         Buffer.add_char buffer ch
       done;
     with BatInnerIO.No_more_input -> ());
    Buffer.contents buffer    
  in
  read_from_binary deserialize filename

(**

See files in binary:

xxd -b filename

**)

(**************************************)
(*              Testing               *)
(**************************************)

let string_serialization_test s = 
  let filename = "text.tmp" in
  write_string_to_binary filename s;
  let s' = read_string_from_binary filename in
  Sys.remove filename;
  s = s'

(* Check that the files are the same *)

let abracadabra = "ABRACADABRA!"

let%test _ = 
  string_serialization_test abracadabra

let string_file_serialization_test source_file = 
  let s = read_file_to_single_string source_file in
  string_serialization_test s

(* Get the file path *)
let find_file fname = 
  Printf.sprintf "%s/%s" (Sys.getcwd ()) fname

let%test _ = 
  let f = (find_file "../../../resources/war-and-peace.txt") in
  string_file_serialization_test f


(************************************)
(*    Enconding DNA sequences       *)
(************************************)

let dna_encoder = function
  | 'A' -> 0
  | 'C' -> 1
  | 'G' -> 2
  | 'T' -> 3
  | _ -> raise (Failure "DNA encoding error")


let dna_decoder = function
  | 0 -> 'A'
  | 1 -> 'C'
  | 2 -> 'G'
  | 3 -> 'T'
  | _ -> raise (Failure "DNA decoding error")

(* 2 bits *)
let dna_encoding_size = 2

let dna_string1 = "CGT"
let dna_string2 = "ATAGATGCATAGCGCATAGCTAGATAGTGCTAG"
let dna_string3 = "ATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGG"
let dna_string4 = "ATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGGATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGGATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGGATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGGATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGGATAGATGCATAGCGCATAGCTAGATAGTGCTAGCGATGCATAGCGCAGATGCATAGCGCAGGGGG"


let write_dna_to_binary filename text = 
  let serialize out text = 
    let size = String.length text in
    write_bits out ~nbits:30 size;
    for i = 0 to size - 1 do
      let ch = dna_encoder text.[i] in
      write_bits out ~nbits:dna_encoding_size ch;
    done
  in
  write_to_binary serialize filename text
    
let read_dna_from_binary filename =  
  let deserialize input = 
    let buffer = Buffer.create 1 in
    let input_length = read_bits input 30 in
    for _ = 0 to input_length - 1 do
      let bits = read_bits input dna_encoding_size in
      let ch = dna_decoder bits in   
      Buffer.add_char buffer ch
    done;
    Buffer.contents buffer
  in
  read_from_binary deserialize filename


let dna_compression_test d = 
  let filename = "dna.tmp" in
  write_dna_to_binary filename d;
  let d' = read_dna_from_binary filename in
  Sys.remove filename;
  d = d'

(*******************************************)
(*          DNA Compression Tests          *)
(*******************************************)

let%test _ = dna_compression_test dna_string1

let%test _ = dna_compression_test dna_string2

let%test _ = dna_compression_test dna_string3

let%test _ = dna_compression_test dna_string4
