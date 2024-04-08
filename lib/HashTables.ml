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

(***********************************************)
(*                 Hashable type               *)
(***********************************************)

module type Hashable = sig
  type t
  val hash : t -> int
end

(***********************************************)
(*    Hashtable signature as a functor         *)
(***********************************************)

module type HashTable = functor 
  (H : Hashable) -> sig
  type key = H.t
  type 'v hash_table
  val mk_new_table : int -> 'v hash_table 
  val insert : (key * 'v) hash_table -> key -> 'v -> unit
  val get : (key * 'v) hash_table -> key -> 'v option
  val remove : (key * 'v) hash_table -> key -> unit
end

(***********************************************)
(*    Concrete hash-table implementation       *)
(***********************************************)

module ListBasedHashTable : HashTable = functor 
  (H : Hashable) -> struct
  type key = H.t

  type 'v hash_table = {
    buckets : 'v list array;
    size : int 
  }

  let mk_new_table size = 
    let buckets = Array.make size [] in
    {buckets = buckets;
     size = size}
  
  let insert ht k v = 
    let hs = H.hash k in
    let bnum = hs mod ht.size in 
    let bucket = ht.buckets.(bnum) in
    let clean_bucket = 
      List.filter (fun (k', _) -> k' <> k) bucket in
    ht.buckets.(bnum) <- (k, v) :: clean_bucket

  let get ht k = 
    let hs = H.hash k in
    let bnum = hs mod ht.size in 
    let bucket = ht.buckets.(bnum) in
    let res = List.find_opt (fun (k', _) -> k' = k) bucket in
    match res with 
    | Some (_, v) -> Some v
    | _ -> None

  (* Slow remove - introduce for completeness *)
  let remove ht k = 
    let hs = H.hash k in
    let bnum = hs mod ht.size in 
    let bucket = ht.buckets.(bnum) in
    let clean_bucket = 
      List.filter (fun (k', _) -> k' <> k) bucket in
    ht.buckets.(bnum) <- clean_bucket
    
end 

(***********************************************)
(*           Testing hash-tables               *)
(***********************************************)

module HashTableIntKey = ListBasedHashTable 
    (struct type t = int let hash i = i end)

let%test _ = 
  let open HashTableIntKey in
  let a = generate_key_value_array 10 in
  let table = mk_new_table 5 in
  Array.iter (fun (k, v) -> insert table k v) a;
  let v = get table (fst a.(0)) |> get_exn in
  Array.exists (fun e -> snd e = v) a
