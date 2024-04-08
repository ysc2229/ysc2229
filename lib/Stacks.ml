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
(*             Representing stacks             *)
(***********************************************)

module type AbstractStack = sig
    type 'e t
    val mk_stack : int -> 'e t
    val is_empty : 'e t -> bool
    val push : 'e t -> 'e -> unit
    val pop : 'e t -> 'e option
  end

(* Simple stack based on OCaml lists *)

module ListBasedStack : AbstractStack = struct
    type 'e t = 'e list ref
    let mk_stack _ = ref []
    let is_empty s = match !s with
      | [] -> true
      | _ -> false
    let push s e = 
      let c = !s in
      s := e :: c
    let pop s = match !s with
      | h :: t ->
        s := t; Some h
      | _ -> None
  end

(* Testing stacks *)

let%test "empty stack" = 
  let open ListBasedStack in
  let s = mk_stack 0 in
  is_empty s 

let%test "empty stack behaviour" = 
  let open ListBasedStack in
  let s = mk_stack 0 in
  push s 123;
  let e = pop s in
  e = Some 123

let%test "complex stack test" = 
  let open ListBasedStack in
  let s = mk_stack 0 in
  push s (4, "aaa");
  push s (5, "bbb");
  push s (7, "ccc");
  not @@ is_empty s

let%test _ = 
  let open ListBasedStack in
  let s = mk_stack 0 in
  let a = generate_int_array 500 in
  Array.iter (fun e -> push s e) a;
  Array.iter (fun e -> let _ = pop s in ()) a;
  is_empty s

(*
# let s = ListBasedStack.mk_stack 0;;
val s : '_weak101 ListBasedStack.t = <abstr>
# push s (4, "aaa");;
- : unit = ()
# push s (5, "bbb");;
- : unit = ()
# push s (7, "ccc");;
- : unit = ()
# is_empty s;;
- : bool = false
# pop s;;
- : (int * string) option = Some (7, "ccc")
# pop s;;
- : (int * string) option = Some (5, "bbb")
# pop s;;
- : (int * string) option = Some (4, "aaa")
# pop s;;
- : (int * string) option = None
# pop s;;
- : (int * string) option = None

*)

(* Array-based stack *)

module ArrayBasedStack : AbstractStack = struct

  type 'e t = {
      elems   : 'e option array;
      cur_pos : int ref 
    }

  let mk_stack n = {
      elems = Array.make n None;
      cur_pos = ref 0
    }
 
 let is_empty s = !(s.cur_pos) = 0

 let push s e = 
   let pos = !(s.cur_pos) in 
   if pos >= Array.length s.elems 
   then raise (Failure "Stack is full")
   else (s.elems.(pos) <- Some e;
         s.cur_pos := pos + 1)
      
    let pop s = 
      let pos = !(s.cur_pos) in
      let elems = s.elems in
      if pos <= 0 then None
      else begin
        let res = elems.(pos - 1) in
        s.elems.(pos - 1) <- None;
        s.cur_pos := pos - 1;
        res
        end
end

(* Testing array-based stack *)

let%test "empty array-based stack" = 
  let open ArrayBasedStack in
  let s = mk_stack 0 in
  is_empty s 

let%test "non-empty array-based stack behaviour" = 
  let open ArrayBasedStack in
  let s = mk_stack 10 in
  push s 123;
  let e = pop s in
  e = Some 123

let%test _ = 
  let open ArrayBasedStack in
  let s = mk_stack 500 in
  let a = generate_int_array 500 in
  Array.iter (fun e -> push s e) a;
  Array.iter (fun e -> let _ = pop s in ()) a;
  is_empty s


(*

# open ArrayBasedStack;;
# let s = mk_stack ();;
val s : '_weak102 ArrayBasedStack.t = <abstr>
# push s (3, "aaa");;
- : unit = ()
# push s (5, "bbb");;
- : unit = ()
# push s (7, "ccc");;
- : unit = ()
# pop s;;
- : (int * string) option = Some (7, "ccc")
# pop s;;
- : (int * string) option = Some (5, "bbb")
# pop s;;
- : (int * string) option = Some (3, "aaa")
# is_empty s;;
- : bool = true
# pop s;;
- : (int * string) option = None

*)
