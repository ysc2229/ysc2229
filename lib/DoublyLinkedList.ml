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
(*             Doubly Linked Lists             *)
(***********************************************)

module DoublyLinkedList = 
  struct
    type 'e dll_node = {
      value : 'e ref;
      prev  : 'e dll_node option ref;
      next  : 'e dll_node option ref
    }

    type 'e t = 'e dll_node option

    let mk_node e = {
      value = ref e;
      prev = ref None;
      next = ref None
    }

    let prev n =  !(n.prev)
    let next n =  !(n.next)
    let value n = !(n.value)
    let set_value n v = n.value := v

    let insert_after n1 n2 = 
      let n3 = next n1 in
      (match n3 with 
       | Some n -> n.prev := Some n2
       | _ -> ());
      n2.next := n3;
      n1.next := Some n2;
      n2.prev := Some n1

    let insert_before n1 n2 = 
      let n0 = prev n2 in
      (match n0 with 
       | Some n -> n.next := Some n1
       | _ -> ());
      n1.prev := n0;
      n1.next := Some n2;
      n2.prev := Some n1

    let rec move_to_head n = 
      match prev n with
      | None -> None
      | Some m -> move_to_head m
      
    let to_list_from n = 
      let res = ref [] in
      let iter = ref (Some n) in
      while !iter <> None do
        let node = (get_exn !iter) in
        res := (value node) :: ! res;
        iter := next node  
      done;
      List.rev !res

    let remove n = 
      (match prev n with
      | None -> ()
      | Some p -> p.next := next n);
      (match next n with
      | None -> ()
      | Some nxt -> nxt.prev := prev n);

  end 

(* Testing *)

let%test "basic node manipulation" = 
  let open DoublyLinkedList in
  let n1 = mk_node (1, "a")
  and n2 = mk_node (2, "b") in
  insert_after n1 n2;
  let n = get_exn @@ next n1 in
  let i = value n in
  let p = get_exn @@ prev n2 in
  let j = value p in
  i = (2, "b") && j = (1, "a")
