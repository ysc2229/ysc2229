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
(*                      Bloom Filters                     *)
(**********************************************************)

(*
Agenda:

1. Design Bloom filters
2. Using for axelerating insertion Simple hash table
3. Using for axelerating fetching from a hash table

*)

module type BloomHashing = sig
  type t
  val hash_functions : (t -> int) list  
end

(* Bloom filter signature *)
module type BloomFilter = functor
  (H: BloomHashing) -> sig
  type t
  val mk_bloom_filter : int -> t
  val insert : t -> H.t -> unit
  val contains : t -> H.t -> bool
  val print_filter : t -> unit
end

(* Bloom filter implementation *)
module BloomFilterImpl : BloomFilter = functor
  (H: BloomHashing) -> struct

  (* Type of filter *)
  type t = {
    slots : bool array;
    size  : int
  }

  let mk_bloom_filter n = 
    let a = Array.make n false in
    {slots = a; size = n}

  let insert f e = 
    let n = f.size in
    List.iter (fun hash ->
        let h = (hash e) mod n in
        f.slots.(h) <- true) H.hash_functions

  let contains f e = 
    if H.hash_functions = [] then false
    else
      let n = f.size in
      let res = ref true in
      List.iter (fun hash ->
          let h = (hash e) mod n in
          res := !res && f.slots.(h)) H.hash_functions;
      !res
        
  module BP = ArrayPrinter(struct
      type t = bool
      let pp b = if b then "1" else "0"
    end)

  let print_filter t = 
    let open BP in
    print_array t.slots
    
end

module IntStringHashing = struct
  type t = int * string
  let hash1 (k, _) = Hashtbl.hash k
  let hash2 (_, v) = Hashtbl.hash v
  let hash3 (k, _) = k 
  let hash_functions = [hash1; hash2; hash3]
end

(* Created bloom filter *)
module IntStringFilter = BloomFilterImpl(IntStringHashing)

(***************************)
(* Testing Bloom filters   *)
(***************************)


(* Now need to test bloom filter *)

let fill_bloom_filter m n = 
  let open IntStringFilter in
  let filter = mk_bloom_filter m in
  let a = generate_key_value_array n in
  for i = 0 to  n - 1 do    
    insert filter a.(i)
  done;
  (filter, a)

let%test "bloom filter true positives" = 
  let open IntStringFilter in
  let fsize = 2000 in
  let len = 1000 in
  let (f, a) = fill_bloom_filter fsize len in 
  for i = 0 to len - 1 do
    assert (contains f a.(i))
  done;
  true

let%test "bloom filter true negatives" = 
  let open IntStringFilter in
  let fsize = 2000 in
  let len = 1000 in
  let (f, a) = fill_bloom_filter fsize len in 
  let al = array_to_list a in

  
  let b = generate_key_value_array len in
  for i = 0 to len - 1 do
    let e = b.(i) in
    if (not (contains f e))
    then assert (not (List.mem e al))
  done;
  true

(********************************************************)
(* Using Bloom filter to speed up the simple hash table *)
(********************************************************)

open BetterHashTable

module BloomHashTable (K: BloomHashing) = struct 
  type key = K.t

  (* Adding bloom filter *)
  module BF = BloomFilterImpl(K)

  type 'v hash_table = {
    buckets : 'v list array;
    capacity : int; 
    filter   : BF.t
  }

  let mk_new_table cap = 
    let buckets = Array.make cap [] in
    (* Pick reasonably large BF size *)
    let filter = BF.mk_bloom_filter 15000 in
    {buckets = buckets;
     capacity = cap;
     filter = filter}
  
  let insert ht k v = 
    let hs = Hashtbl.hash k in
    let bnum = hs mod ht.capacity in 
    let bucket = ht.buckets.(bnum) in
    let filter = ht.filter in
    let clean_bucket = 
      (* New stuff *)
      if BF.contains filter k
      (* Only filter if ostensibly contains key *)
      then List.filter (fun (k', _) -> k' <> k) bucket 
      else bucket in
    (* Missed in the initial the implementation *)
    BF.insert filter k;
    ht.buckets.(bnum) <- (k, v) :: clean_bucket

  let get ht k = 
    let filter = ht.filter in
    if BF.contains filter k then
      let hs = Hashtbl.hash k in
      let bnum = hs mod ht.capacity in 
      let bucket = ht.buckets.(bnum) in
      let res = List.find_opt (fun (k', _) -> k' = k) bucket in
      match res with 
      | Some (_, v) -> Some v
      | _ -> None
    else None

  (* Cannot remove *)
  let remove _ _ = raise (Failure "Removal is deprecated!")

  let print_hash_table ppk ppv ht = 
    let open Printf in
    print_endline @@ sprintf "Capacity: %d" (ht.capacity);
    print_endline "Buckets:";
    let buckets = (ht.buckets) in
    for i = 0 to (ht.capacity) - 1 do
      let bucket = buckets.(i) in
      if bucket <> [] then (
        (* Print bucket *)
        let s = List.fold_left 
            (fun acc (k, v) -> acc ^ (sprintf "(%s, %s); ") (ppk k) (ppv v)) "" bucket in
        printf "%d -> [ %s]\n" i s)
    done
end

(* Testing Bloomfilter hash table *)


module BHT = BloomHashTable(IntStringHashing)
module BHTTester = HashTableTester(BHT)

let insert_and_get_bulk_bloom a m = 
  Printf.printf "Creating Bloom hash table:\n";
  let ht = time (BHTTester.mk_test_table_from_array_length a) m in
  Printf.printf "Fetching from Bloom hash table on the array of size %d:\n" (Array.length a);
  let _ = time BHTTester.test_table_get ht a in ()

let compare_hashing_time_simple_bloom n m = 
  let a = generate_key_value_array n in
  insert_and_get_bulk_simple a m;
  print_endline "";
  insert_and_get_bulk_bloom a m

(* Try:

compare_hashing_time_simple_bloom 15000 20;;

*)


(***************************)
(*   Testing Bloom tables  *)
(***************************)

let%test "BloomHashTable insert" = 
  let open BHTTester in
  let a = generate_key_value_array 1000 in
  let ht = mk_test_table_from_array_length a 50 in
  test_table_get ht a


