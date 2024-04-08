(*
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

include Polygons
include Stacks

(*************************************)
(*        Auxiliary opoerations      *)
(*************************************)

module StackX (S: AbstractStack) = struct
  include S

  let top s = match pop s with
    | None -> None
    | Some x ->
      push s x;
      Some x

  let next_to_top s = match pop s with
    | None -> None
    | Some x -> 
      let y = top s in
      push s x;
      y

  let list_of_stack s = 
    let res = ref [] in
    while not (is_empty s) do
      let e = get_exn @@ pop s in
      res := e :: !res
    done;
    !res

end

(*************************************)
(*           Convex hull             *)
(*************************************)

(* Sort by axis Y *)
let axis_y_sorter (Point (x1, y1)) (Point (x2, y2)) =
  if y1 < y2 then -1 else if y1 > y2 then 1
  else if x1 < x2 then -1 else if x1 > x1 then 1
  else 0

(* Sort by polar angle wrt p0 *)
let polar_angle_sorter p0 p1 p2 = 
  let Polar (r1, a1) = p1 -- p0 |> polar_of_cartesian in
  let Polar (r2, a2) = p2 -- p0 |> polar_of_cartesian in
  if a1 < a2 then -1 else if a1 > a2 then 1
  else if r1 < r2 then -1 else if r1 > r2 then 1 
  else 0

module CHStack = StackX(ListBasedStack)

(* Graham's Scan *)
let convex_hull points = 
  (* At least three points *)
  assert (List.length points >= 3);

  let y_sorted = List.sort axis_y_sorter points in
  let p0 = y_sorted |> List.hd in 
  match List.tl y_sorted |> 
          List.sort (polar_angle_sorter p0) with
   | p1 :: p2 :: rest -> 
      let open CHStack in
      let s = mk_stack 0 in
      push s p0;
      push s p1;
      push s p2; 
      
      let non_left_turn p = 
        let q1 = next_to_top s |> get_exn in
        let q2 = top s |> get_exn in
        direction q1 q2 p >= 0
      in
      
      (* Main algorithm *)
      List.iter (fun p ->
          while non_left_turn p do
            ignore (pop s)
          done;
          push s p) rest;
      
      list_of_stack s 
   | _ -> error "Cannot happen"

(* Question: what is the complexity *)
    
(*************************************)
(*        Testing Convex hulls       *)
(*************************************)

let gen_random_points ?dim:(dim = 550.) n = 
  let res = ref [] in
  for _ = 0 to n - 1 do
    let p = gen_random_point dim in
    res := p :: !res
  done;
  !res
    

(*************************************)
(*     Tracing convex hulls          *)
(*************************************)

let draw_stack_and_points s ps p = 
  let open CHStack in
  if is_empty s then ()
  else begin
    let l = list_of_stack s in 
    List.iter (fun e -> push s e) l;
    let ll = all_pairs l in
    List.iter draw_point ps;
    List.iter (draw_segment ~color:Graphics.red) ll;
    Unix.sleepf 0.3
  end
  
let convex_hull_with_tracing ?cur:(cur = false) points = 
  (* At least three points *)
  assert (List.length points >= 3);

  let y_sorted = List.sort axis_y_sorter points in
  let p0 = y_sorted |> List.hd in 
  match List.tl y_sorted |> 
          List.sort (polar_angle_sorter p0) with
   | p1 :: p2 :: rest -> 
      let open CHStack in
      let s = mk_stack 0 in
      push s p0;
      push s p1;
      push s p2; 
      
      let non_left_turn p = 
        let q1 = next_to_top s |> get_exn in
        let q2 = top s |> get_exn in
        direction q1 q2 p >= 0
      in
      
      List.iter (fun p ->
          while non_left_turn p do
            let _ = pop s in ()
          done;
          push s p;
          
      clear_screen ();    
      (if cur then draw_segment ~color:Graphics.blue (p0, p));
      draw_stack_and_points s points p) rest;
      
      let res = list_of_stack s in
      
      clear_screen ();
      List.iter draw_point points;
      draw_polygon ~color:Graphics.red res;
      
      res
   | _ -> error "Cannot happen"

(*****************************************)
(*        Testing Convex Hulls           *)
(*****************************************)

let test_random_ch n = 
  let ps = gen_random_points n in 
  let ch = convex_hull ps in
  assert (is_convex ch);
  assert (List.for_all (point_within_polygon ch) ps)

let%test _ = 
  for _ = 0 to 100 do
    test_random_ch 50
  done;
  true
  
let%test _ = 
  let p = 
    [Point (0., 0.); Point (6., 0.); Point (6., 1.); 
     Point (8., 1.); Point (8., 2.); Point (6., 2.); 
     Point (6., 3.); Point (0., 3.)] in
  point_within_polygon p @@ Point (1.,1.)



