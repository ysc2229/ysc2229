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
include Paths

(***********************************)
(* Interpreting Undirected Graphs  *)  
(***********************************)
  
let graphviz_with_weights_undirected g out = 
  let open AdjacencyGraphs in
  let ag = LinkedGraphs.to_adjacency_graph g in
  let vattrib = get_ag_node_payload ag in
  let eattrib (s, d) = 
    let l = get_ag_edge_label ag s d |> string_of_int in
    Printf.sprintf "[label=\"%s\", weight=\"%s\"]" l l
  in
  let s = graphviz_string_of_graph "graph" " -- " 
      vattrib eattrib ag in
  write_string_to_file out s


(***********************************)
(*        Kruskal's algorithm      *)  
(***********************************)

open UnionFind
open LinkedGraphs


let mst_kruskal g = 
  let open UnionFind in
  let forest = mk_UF (v_size g) in
  let tree = ref [] in

  let edges_sorted = 
    Set.elements g.edges |>
      List.sort (fun (a, b) (x, y) -> 
        let w1 = get_linked_edge_label g a b in
        let w2 = get_linked_edge_label g x y in
        if w1 < w2 then -1 else if w1 > w2 then 1 else 0) in

  List.iter (fun (u, v) ->
      let su = find forest u in
      let sv = find forest v in
      if su <> sv 
      then begin
        tree := (u, v) :: !tree;
        union forest u v
      end) edges_sorted;

  !tree

(***********************************)
(*           Rendering MST         *)  
(***********************************)

let graphviz_with_mst algo g out = 
  let t = algo g in 
  let attrib (u, v) = 
    let l = get_linked_edge_label g u v |> string_of_int in
    let b = List.exists 
        (fun (x, y) -> x = u && y = v || x = v && y = u) t in
    if b then
      Printf.sprintf "[label=\"%s\", color=red,penwidth=3.0]" l
    else
      Printf.sprintf "[label=\"%s\"]" l
  in
  let ag = LinkedGraphs.to_adjacency_graph g in
  let s = graphviz_string_of_graph "graph" " -- " 
      (get_linked_node_payload g) attrib ag in
  write_string_to_file out s

let graphviz_with_kruskal =
  graphviz_with_mst mst_kruskal

(***********************************)
(*           Examples              *)  
(***********************************)

(*   Undirected Example   *)
  
let undirected_example_nodes = 
  [| 
    "a";
    "b";
    "c";
    "d";
    "e";
    "f";
    "g";
    "h";
    "i";
  |]

let undirected_example_edges = 
  [
    (0, 1);
    (0, 7);
    (1, 2);
    (1, 7);
    (2, 3);
    (2, 5);
    (2, 8);
    (3, 4);
    (3, 5);
    (4, 5);
    (5, 6);
    (6, 7);
    (6, 8);
    (7, 8)
  ]

let undirected_example_labels = 
  [
    (0, 1, 4);
    (0, 7, 8);
    (1, 2, 8);
    (1, 7, 11);
    (2, 3, 7);
    (2, 5, 4);
    (2, 8, 2);
    (3, 4, 9);
    (3, 5, 14);
    (4, 5, 10);
    (5, 6, 2);
    (6, 7, 1);
    (6, 8, 6);
    (7, 8, 7)
  ]

let example_graph_undirected = 
  read_graph_and_payloads 9 undirected_example_nodes
    undirected_example_edges undirected_example_labels

let%test "Testing MST size" = 
  let t = mst_kruskal example_graph_undirected in 
  List.length t = v_size example_graph_undirected - 1
