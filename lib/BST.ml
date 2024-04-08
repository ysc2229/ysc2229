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

(**********************************)
(*     Binary Search Trees        *)
(**********************************)

module BinarySearchTree = struct

(*

Supported operations:

* Get minimum
* Get maximum
* Find an element
* Find a successor
* Insert an element
* Delete an element

*)

  (**********************************)
  (*    1.  Defining a tree         *)
  (**********************************)

  type 'e tree_node = {
    value : 'e;
    parent  : 'e tree_node option ref;
    left  : 'e tree_node option ref;
    right  : 'e tree_node option ref;
  }

  type 'e tree = {
    root : 'e tree_node option ref;
    size : int ref
  }

  let left n = !(n.left)
  let right n = !(n.right)
  let parent n = !(n.parent)
  let get_root t = !(t.root)
  let get_size t = !(t.size)

  let mk_node e = 
    {value = e;
     parent = ref None;
     left = ref None;
     right = ref None}
    
  let mk_tree _ = {root = ref None; size = ref 0}    
    
  let map_option o f z = match o with
    | None -> z
    | Some n -> f n
      

  (**********************************)
  (*     2. Growing the tree        *)
  (**********************************)
    
  let insert t e =       
    let rec insert_element n e = 
      let m = mk_node e in
      if e < n.value
      then match left n with
        | Some m -> insert_element m e
        | None ->
          m.parent := Some n;
          n.left := Some m;
          true
      else if e > n.value
      then match right n with
        | Some m -> insert_element m e
        | None ->
          m.parent := Some n;
          n.right := Some m;
          true
      else false
    in
    match !(t.root) with
    | None -> (
        t.root := Some (mk_node e);
        t.size := 1;
        true)
    | Some n -> 
      if insert_element n e
      then (t.size := !(t.size) + 1; true)
      else false
                     
              
  (**********************************)
  (*     2.5 Tree invariant         *)
  (**********************************)

  let check_bst_inv t = 
    let rec walk node p = 
      (p node.value) &&
      let res_left = match left node with
        | None -> true
        | Some l -> walk l (fun w -> p w && w < node.value)
      in
      let res_right = match right node with
        | None -> true
        | Some r -> walk r (fun w -> p w && w > node.value)
      in
      res_left && res_right
    in
    match !(t.root) with
    | None -> true
    | Some n -> walk n (fun _ -> true)

  (**********************************)
  (*     3. Printing a tree         *)
  (**********************************)

  let print_tree pp snum t = 
    let print_node_with_spaces l s = 
      for i = 0 to s - 1 do 
        Printf.printf " "
      done;
      print_endline (pp l.value);
    in

    let rec walk s node = match node with
      | None -> ()
      | Some n -> begin
          walk (s + snum) (right n);
          print_node_with_spaces n s;
          walk (s + snum) (left n);
        end      
    in
    map_option (get_root t) (fun n -> walk 0 (Some n)) ()
    
  (**********************************)
  (*     4. Exploring the tree      *)
  (**********************************)

  let search t k = 
    let rec walk k n = 
      let nk = n.value in 
      if k = nk then Some n
      else if k < nk
      then match left n with
        | None -> None
        | Some l -> walk k l
      else match right n with
        | None -> None
        | Some r -> walk k r
    in
    map_option (get_root t) (walk k) None

  (**********************************)
  (* 5. Traversing a tree with DFS  *)
  (**********************************)

  open Stacks
  open Queues
  open DLLBasedQueue

  let depth_first_search_rec t = 
    let rec walk q n =
      (match left n with
       | Some l -> walk q l
       | None -> ());
      enqueue q n.value;
      (match right n with
       | Some r -> walk q r
       | None -> ());
    in
    let acc = (mk_queue 0) in
    map_option (get_root t) (walk acc) ();
    queue_to_list acc

  (**********************************)
  (* 6. Traversing a tree with BFS  *)
  (**********************************)

  let breadth_first_search_loop t = 
    let loop wlist q depth =
      while not (is_empty wlist) do
        let n = get_exn @@ dequeue wlist in
        (match left n with
         | Some l -> enqueue wlist l
         | _ -> ());
        enqueue q n.value;
        (match right n with
         | Some r -> enqueue wlist r
         | _ -> ());
      done
    in
    let acc = (mk_queue 0) in
    let wlist = mk_queue 0 in
    (match get_root t with
    | None -> ()
    | Some n -> begin
        enqueue wlist n;
        loop wlist acc 0;
      end);      
    queue_to_list acc

  let elements t = breadth_first_search_loop t

  (**********************************)
  (* 7.  Finding a minimum node     *)
  (**********************************)

  let rec find_min_node n = 
    match left n with
    | Some m -> find_min_node m
    | None -> n

  (* Question: how to find a successor of a node in a tree? *)

  (**********************************)
  (* 8.    Deletion of an element   *)
  (**********************************)

  (* Replacing node U by (optional) node V in T. *)
  let transplant t u v = 
    (match parent u with
    | None -> t.root := v
    | Some p -> 
      match left p with
      | Some l when u == l -> p.left := v
      | _ -> p.right := v);
    (* Update parent of v *)
    match v with 
    | Some n -> n.parent := parent u
    | _ -> ()

  (* Deleting the a node z from tree *)
  (* z must be in the tree *)
  let delete_node t z = 
    t.size := !(t.size) - 1;
    if left z = None
    then transplant t z (right z)
    else if right z = None
    then transplant t z (left z)
    else
      (* Finding the successor of `z` *)
      let z_right_child = (get_exn @@ right z) in
      let y = find_min_node z_right_child in
      (* Fact: `y` has no left child *)

      (if parent y <> None &&
          z != get_exn @@ parent y
       then 
      (*  If y is not immediately under z,
          replace y by its right subtree *)
         let x = right y in
         (transplant t y x;
          y.right := right z;
          (get_exn @@ right y).parent := Some y));

      (* Now `y` replaces `z` at its position *)
      transplant t z (Some y);
      y.left := !(z.left);
      (get_exn @@ left y).parent := Some y
  

  (**********************************)
  (* 9. Rotations and balanced tree *)
  (**********************************)

  let left_rotate t x =
    match right x with
    | None -> ()
    | Some y ->
      
      (* turn y's left subtree into x's right subtree *)
      x.right := left y;
      (if left y <> None
       then (get_exn @@ left y).parent := Some x);
      
      (* link x's parent to y *)
      y.parent := parent x;

      (match parent x with 
       | None -> t.root := Some y
       | Some p -> match left p with
         | Some l when x == l ->
           p.left := Some y
         | _ ->
           p.right := Some y);
            
      (* Make x the left child of y *)
      y.left := Some x;
      x.parent := Some y      
end

(***********************************************************)
(***********************************************************)
(***********************************************************)

(* Experiments with printing the tree *)
open BinarySearchTree

module SimpleTree = struct 

  let elems = [15; 10; 20; 8; 12; 16; 25]

  let mk_simple_tree _ = 
    let t = mk_tree () in
    List.iter (fun e -> ignore (insert t e)) elems;
    t

end

module TestTree = struct

  let mk_test_tree _ =     
    let a = [|
        (6, "sjphh");
        (1, "huoab");    
        (7, "ixfow");
        (3, "qtvyn");
        (4, "wjcna");
        (2, "ccxym");
        (5, "wikqt");
        (2, "nzbiv");    
      |]
    in
    let t = mk_tree () in
    for i = 0 to 7 do 
      assert (insert t a.(i))
    done;
    t

end

let print_kv_tree = print_tree 
    (fun (k, v) -> Printf.sprintf "(%d, %s)" k v) 12

let mk_tree_of_size n =
  let t = mk_tree () in
  let a = generate_key_value_array n in
  for i = 0 to n - 1 do 
    assert (insert t a.(i))
  done;
  assert (get_size t = n);
  t


(******************************************)
(*          Testing insertion             *)
(******************************************)

let%test "Testing insertion" = 
  let n = 1000 in
  let t = mk_tree_of_size n in
  check_bst_inv t

(******************************************)
(*          Testing Search                *)
(******************************************)

let check_elem_in_tree t e = 
  let n = search t e in
  (get_exn @@ n).value = e

let%test "Testing search" = 
  let n = 1000 in
  let t = mk_tree () in
  let a = generate_key_value_array n in
  for i = 0 to n - 1 do 
    assert (insert t a.(i))
  done;
  for i = 0 to n -1 do
    assert (search t a.(i) <> None)
  done;
  true

(******************************************)
(*          Testing traversals            *)
(******************************************)

let%test "Testing DFS" = 
  let n = 1000 in
  let t = mk_tree_of_size n in
  let l1 = depth_first_search_rec t in
  List.length l1 = n &&
  List.for_all (fun e -> check_elem_in_tree t e) l1 &&
  sorted l1

let%test "Testing BFS" = 
  let n = 1000 in
  let t = mk_tree_of_size n in
  let l1 = depth_first_search_rec t in
  let l2 = breadth_first_search_loop t in
  List.length l1 = n && 
  List.for_all (fun e -> List.mem e l2) l1 &&
  List.for_all (fun e -> List.mem e l1) l2

(******************************************)
(*          Testing retrieval             *)
(******************************************)

let%test "Testing retrieval" = 
  let n = 1000 in
  let t = mk_tree_of_size n in
  let m = Random.int n in
  let l = breadth_first_search_loop t in
  let e = List.nth l m in
  let z = search t e in
  z <> None

(******************************************)
(*          Testing deletion              *)
(******************************************)

let test_delete n = 
  let t = mk_tree_of_size n in
  let m = Random.int n in
  let l = breadth_first_search_loop t in
  let e = List.nth l m in
  let z = get_exn @@ search t e in
  delete_node t z;
  (* Checking the tree invariant *)
  assert (check_bst_inv t);

  (* Checking the tree size *)
  let ld = breadth_first_search_loop t in
  assert (List.length ld = n - 1);

  (* Checking integrity *)
  assert (List.for_all (fun x -> List.mem x ld || x == e) l)


let%test "Testing deletion" = 
  for i = 1 to 100 do
    test_delete 1000
  done;
  true
