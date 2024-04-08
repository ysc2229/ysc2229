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

open ArrayUtil

module UnionFind = struct
  
  type t = {
    count : int ref;
    id : int array
  }

  let mk_UF n = 
    let ints = 
      list_to_array (iota (n - 1)) in
    { count = ref n;
      id = ints }

  let get_count uf = !(uf.count)

  (* Question: What is the complexity of find? *)
  let find uf p = 
    let r = ref p in 
    while (!r <> uf.id.(!r)) do
      r := uf.id.(!r)
    done;
    !r

  (* Question: What is the complexity of union? *)
  let union uf p q = 
    let i = find uf p in
    let j = find uf q in
    if i = j then ()
    else begin
      uf.id.(i) <- j;
      uf.count := !(uf.count) - 1
    end

  let connected uf p q =
    find uf p = find uf q

  let print_uf uf = 
    let n = Array.length uf.id in
    let ids = iota (n - 1) in
    for i = 0 to n - 1 do
      let connected = List.find_all (fun e -> find uf e = i) ids in
      if connected <> [] then begin
        Printf.printf "Class %d: [" i;
        List.iter (fun j -> Printf.printf "%d; " j) connected;
        print_endline "]"
      end      
    done                      
end

(******************************************)
(*          Testing Union-Find            *)
(******************************************)

let%test "UF" =
  let open UnionFind in
  let uf = mk_UF 10 in
  union uf 0 1;
  union uf 2 3;
  union uf 4 5;
  union uf 6 7;
  union uf 8 9;
  assert (get_count uf = 5);
  assert (connected uf 0 1);

  assert (not (connected uf 6 8));
  assert (not (connected uf 0 9));

  union uf 1 3;
  union uf 5 7;
  union uf 5 9;
  assert (get_count uf = 2);
  assert (connected uf 6 8);

  union uf 3 4;
  assert (get_count uf = 1);
  assert (connected uf 0 9);

  true
