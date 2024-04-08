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

open ReadingFiles
open BST
open BinarySearchTree
include Reachability

(***********************************)
(*       Rendering digraphs        *)  
(***********************************)

let get_ag_node_payload ag n = 
  let open AdjacencyGraphs in
  List.find (fun (x, _) -> x = n) !(ag.node_payloads) |> snd

let get_ag_edge_label ag s d = 
  let open AdjacencyGraphs in
  List.find (fun ((x, y), _) -> s = x && y = d) 
    !(ag.edge_labels) |> snd

let get_linked_edge_label g s d = 
  let open LinkedGraphs in
  EdgeTable.get g.edge_labels (s, d) |> get_exn

let get_linked_node_payload g n = 
  let open LinkedGraphs in
  let node = NodeTable.get g.node_map n |> get_exn in
  !(node.value)

let graphviz_with_weights g out = 
  let open AdjacencyGraphs in
  let ag = LinkedGraphs.to_adjacency_graph g in
  let vattrib = get_ag_node_payload ag in
  let eattrib (s, d) = 
    let l = get_ag_edge_label ag s d |> string_of_int in
    Printf.sprintf "[label=\"%s\", weight=\"%s\"]" l l
  in
  let s = graphviz_string_of_graph "digraph" " -> " 
      vattrib eattrib ag in
  write_string_to_file out s


(***********************************)
(*            Distance             *)  
(***********************************)

module Distance = struct

type dist = 
    | Finite of int
    | Infinity

let (<) d1 d2 = match (d1, d2) with
  | Infinity, _ -> false 
  | Finite _, Infinity -> true
  | Finite x, Finite y -> x < y

let (<=) d1 d2 = d1 < d2 || d1 = d2
let (>) d1 d2 = not (d1 <= d2)
let (>=) d1 d2 = not (d1 < d2)


let (+) d1 d2 = match (d1, d2) with
  | Infinity, _ -> Infinity
  | _, Infinity -> Infinity
  | Finite x, Finite y -> Finite (x + y)

let int_of_dist d = match d with
  | Infinity -> raise (Failure "Cannot convert infinity to integer!")
  | Finite n -> n

end

(***********************************)
(*       Main operations for SSP   *)  
(***********************************)

open LinkedGraphs
open NodeTable

(* Initialise single source *)
let initialise_single_source g s = 
  let open Distance in
  let n = v_size g in 
  let dist_table = mk_new_table n in
  let prev_tree  = mk_new_table n in
  for i = 0 to n - 1 do
    insert dist_table i Infinity;
  done;
  insert dist_table s (Finite 0);
  let w = get_linked_edge_label g in
  (w, dist_table, prev_tree)

(* Get distance from the table *)
let dist dist_table u =
  let open NodeTable in 
  get_exn @@ get dist_table u

(* Relax the distance between u and v *)
let relax dist_table prev_tree w u v = 
  let open Distance in
  let vud = dist dist_table u + (Finite (w u v)) in 
  if dist dist_table v > vud
  then begin
    insert dist_table v vud;
    insert prev_tree v u
  end


(***********************************)
(*     Bellman-Ford algorithm      *)  
(***********************************)

let bellman_ford g s = 
  let open Distance in 
  let (w, d, p) = initialise_single_source g s in
  let all_edges = elements g.edges in 

  for i = 0 to v_size g - 1 do 
    List.iter (fun (u, v) -> relax d p w u v) all_edges 
  done;
  
  (* Check for negative cycles *)
  let rec check_neg_cycles es = match es with
    | [] -> true
    | (u, v) :: t ->
      if dist d v > dist d u + (Finite (w u v))
      then false
      else check_neg_cycles t 
  in
  
  ((p, d), check_neg_cycles all_edges)

(*

Question: Why check_neg_cycles works? 

Question: What is a complexity of bellman_ford in terms of G.V and G.E?

*)

(***********************************)
(*   Renderring Graphs with Paths  *)  
(***********************************)

let graphviz_with_min_paths path_calculuator g s out = 
  let p = path_calculuator g s in 
  let attrib (u, v) = 
    let l = get_linked_edge_label g u v |> string_of_int in
    match get p v with
    | Some z when u = z -> 
      Printf.sprintf "[label=\"%s\", color=red,penwidth=3.0]" l
    | _ -> 
      Printf.sprintf "[label=\"%s\"]" l
  in
  let ag = LinkedGraphs.to_adjacency_graph g in
  let s = graphviz_string_of_graph "digraph" " -> " 
      (get_linked_node_payload g) attrib ag in
  write_string_to_file out s


(* Render graph with Bellman-Ford *)
let graphviz_with_bellman_ford = 
  let pc g s = bellman_ford g s |> fst |> fst in
  graphviz_with_min_paths pc

(* Try it on example_graph_bf *)

(*     Bellman-Ford Example   *)  
let bf_example_nodes = 
  [| 
    "s";
    "t";
    "y";
    "x";
    "z"
  |]

let bf_example_edges = 
  [
    (0, 1);
    (0, 2);
    (1, 2);
    (1, 3);
    (1, 4);
    (2, 3);
    (2, 4);
    (3, 1);
    (4, 0);
    (4, 3)
  ]

let bf_example_labels = 
  [
    (0, 1,  6);
    (0, 2,  7);
    (1, 2,  8);
    (1, 3,  5);
    (1, 4, -4);
    (2, 3, -3);
    (2, 4,  4);
    (3, 1, -2);
    (4, 0,  2);
    (4, 3,  7)
  ]
  
(* Bellman-Ford example graph *)
let example_graph_bf = 
  read_graph_and_payloads 5 bf_example_nodes
    bf_example_edges bf_example_labels

(***********************************)
(*       Dijkstra algorithm        *)  
(***********************************)

(* Extract minimal distance in O(|remaining|) *)
let extract_min_dist dist_table remaining = 
  let open Distance in
  let res = ref None in
  let d = ref Infinity in
  List.iter (fun i ->
      let di = dist dist_table i in
      if di <= !d
      then begin
        res := Some i;
        d := di
      end) !remaining;

  match !res with
  | None -> None
  | Some i -> begin
      remaining := List.filter (fun j -> i <> j) !remaining;
      !res
    end
      
let dijkstra g s = 
  let (w, d, p) = initialise_single_source g s in

  (* Make queue of remaining uninspected nodes *)
  let q = ref (iota (v_size g - 1)) in
  while !q <> [] do
    let u = extract_min_dist d q |> get_exn in
    let adj = get_succ g u in
    List.iter (fun v -> relax d p w u v) adj
  done;
  (p, d)

(*

Question: Why Dijkstra relies on non-negative paths?

Hint: Think about local and global optimality.

*)
    
let graphviz_with_dijkstra = 
  let pc g s = dijkstra g s |> fst in
  graphviz_with_min_paths pc

(* Try it on example_graph_dijkstra *)

(*     Dijkstra Example   *)

let dijkstra_example_nodes = 
  [| 
    "s";
    "t";
    "y";
    "x";
    "z"
  |]

let dijkstra_example_edges = 
  [
    (0, 1);
    (0, 2);
    (1, 2);
    (1, 3);
    (2, 1);
    (2, 3);
    (2, 4);
    (3, 4);
    (4, 0);
    (4, 3);
  ]

let dijkstra_example_labels = 
  [
    (0, 1, 10);
    (0, 2, 5);
    (1, 2, 2);
    (1, 3, 1);
    (2, 1, 3);
    (2, 3, 9);
    (2, 4, 2);
    (3, 4, 4);
    (4, 0, 7);
    (4, 3, 6);
  ]

let example_graph_dijkstra = 
  read_graph_and_payloads 5 dijkstra_example_nodes
    dijkstra_example_edges dijkstra_example_labels


(***********************************)
(*   Working with Shortest Paths   *)  
(***********************************)

let get_shortest_path p s u = 
  let rec walk acc v = match get p v with
    | None -> acc
    | Some x -> walk ((x, v) :: acc) x
  in
  let res = walk [] u in
  if u = s || 
     res <> [] && (List.hd res |> fst = s)
  then Some res
  else None

let rec get_path_weigth g path = match path with
  | (u, v) :: t -> 
    let w = get_linked_edge_label g u v in 
    w + get_path_weigth g t
  | _ -> 0

(***************************************)
(*    Testing for SS shortest paths    *)
(***************************************)

(*

What is provided for testing:

* p - predecessor tree
* d - distance table
* g - the graph
* s - source node
* u - destination node

*)

(* 1. Path is connected *)
let test_path_connected p d g s u = 
  match get_shortest_path p s u with
  | None -> true
  | Some path ->
    let rec walk p = match p with
      | (u, v) :: (x, y) :: t ->
        v = x && walk ((x, y) :: t )
      | _ -> true
    in
    walk path

(* 2. Path's weight is correctly recorded *)
let test_path_weight p d g s u =
  match get_shortest_path p s u with
  | None -> true
  | Some path ->
    let w1 = get_path_weigth g path in 
    let w2 = get_exn @@ get d u |> Distance.int_of_dist in
    w1 = w2

(* 3. Has all edges *)    
let test_that_is_path_graph p d g s u =
  match get_shortest_path p s u with
  | None -> true
  | Some path ->
    let all_edges = g.edges |> elements in
    List.for_all (fun e -> List.mem e all_edges) path 

(* 4. Exists for any reachable node *)
let test_reachable_hence_has_path p d g s u = 
  if is_reachable g s u 
  then get_shortest_path p s u <> None
  else true

(* 5. And is the shortest *)
let test_shortest_is_shorter p d g s u = 
  match reachable g s u with
  | None -> true
  | Some p1 ->
    match get_shortest_path p s u with
    | None -> false
    | Some p2 ->
      let w1 = get_path_weigth g p1 in 
      let w2 = get_path_weigth g p2 in 
      w2 <= w1

(*  Main testing function  *)

let test_sssp algo g = 
  let all_nodes = get_nodes g in
  List.iter (fun u ->
      List.iter (fun v ->
          let (p, d) = algo g u in
          assert (test_path_connected p d g u v);
          assert (test_path_weight p d g u v);
          assert (test_that_is_path_graph p d g u v);
          assert (test_reachable_hence_has_path p d g u v);
          assert (test_shortest_is_shorter p d g u v);
        ) all_nodes) all_nodes;
  true


(***********************************)
(*           Testing               *)  
(***********************************)
  
(*  Testing Bellman-Ford  *)

let%test "Bellman-Ford-1" = 
  let algo g s = bellman_ford g s |> fst in
  test_sssp algo example_graph_bf
  
(*  Testing Dijkstra  *)

let%test "Dijkstra" = 
  test_sssp dijkstra example_graph_dijkstra

let%test "Bellman-Ford-2" = 
  let algo g s = bellman_ford g s |> fst in
  test_sssp algo example_graph_dijkstra
  

