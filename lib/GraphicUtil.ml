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

open Graphics

let origin = (400, 300)

let go_to_origin _ =
  let x = fst origin in
  let y = snd origin in
  moveto x y;
  set_color black

let draw_axes _ =
  let x = fst origin in
  let y = snd origin in
  set_color green;
  moveto 0 y;
  lineto (x * 2) y;
  moveto x 0;
  lineto x (y * 2);
  moveto x y;
  set_color black

let mk_screen _ = 
  open_graph " 800x600";
  draw_axes ()
    
let clear_screen _ =
  clear_graph ();
  draw_axes ()

