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

(*************************************)
(*    Computing Fibonacci numbers    *)
(*************************************)

let rec naive_fib n = 
  if n <= 1 then 1 
  else naive_fib (n - 1) + naive_fib (n - 2)


(* With memoization *)

let rec memo_fib n = 
  if n <= 1 then 1 
  else begin
    let fib = ref  1 in
    let fib_prev = ref 1 in
    for i = 2 to n do
      let tmp = !fib_prev in 
      fib_prev := !fib;
      fib := tmp + !fib;
    done; 
    !fib
  end

(* Testing Fibonacci numbers *)

let test_fib fib_fun n = 
  for i = 0 to n do
    assert (fib_fun n = naive_fib n)
  done;
  true

(* Compare time *)
let%test _ = test_fib memo_fib 25


(*************************************)
(*        Knapsack problem           *)
(*************************************)

(* name * weight * price *)
let fruit_sack = [|
  ("apple",  1, 1);
  ("melon",  2, 2);
  ("kiwi",   1, 2);
  ("durian", 2, 3);
|]

(* Utility functions *)
let weight items i = 
  let (_, w, _) = items.(i) in w

let price items i = 
  let (_, _, p) = items.(i) in p

let allowed_weight = 4

(*************************************)
(*       Determining the weight      *)
(*************************************)

let knapsack_max_price max_weight items = 
  let num_items = Array.length items in 
  (* n - currently observed item
     w - remaining weight        *)
  let rec solver n w = 
    if n < 0 || w == 0 then 0
    else 
      let wn = weight items n in
      if wn > w 
      then solver (n - 1) w
      else
        let option1 = solver (n - 1) w in
        let pn = price items n in    
        let option2 = pn + solver (n - 1) (w - wn) in
        max option1 option2
  in
  solver (num_items - 1) max_weight

let%test _ = 
  knapsack_max_price 4 fruit_sack = 6

(****************************************)
(*   Solving via dynamic programming    *)
(****************************************)

let knapsack_max_price_dynamic max_weight items = 
  let num_items = Array.length items in 

  (* Make array of maximal prices 
     m.(i).(w) = max price when taking up to i items 
                 with max weight w *)

  let m = Array.make (num_items + 1) [||] in
  for i = 0 to num_items do
    m.(i) <- Array.make (max_weight + 1) 0
  done;

  (* Main operation *)
  for i = 1 to num_items do
      for w = 1 to max_weight do
        if weight items (i - 1) <= w 
        then
          let p = price items (i - 1) in
          m.(i).(w) <- max 
              (m.(i - 1).(w))
              (m.(i - 1).(w - weight items (i - 1)) + p)
        else m.(i).(w) <- m.(i - 1).(w)
      done
  done;

  (m.(num_items).(max_weight), m)

let%test _ = 
  let (p, _) = knapsack_max_price_dynamic 4 fruit_sack in
  p = 6


(*

Results

n  item    w  p |  
--------------------------------
0  apple   1  1 |  0  1  1  1  1  
1  melon   2  2 |  0  1  2  3  3  
2  kiwi    1  2 |  0  2  3  4  5  
3  durian  2  3 |  0  2  3  5  6 

*)

(****************************************)
(*    Testing two implementations       *)
(****************************************)


open ArrayUtil

let generate_pos_nums n m = 
   generate_keys m n |> 
   List.map (fun e -> e + 1)

(* 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list *)
let list_zip3 ls1 ls2 ls3 = 
  let rec walk xs1 xs2 xs3 k = match xs1, xs2, xs3 with
    | h1 :: t1, h2 :: t2, h3 :: t3 -> 
      walk t1 t2 t3 (fun acc -> k ((h1, h2, h3) :: acc))
    | _ -> k []
  in
  walk ls1 ls2 ls3 (fun x -> x)    

let generate_basket n = 
   let names = generate_words 5 n in
   let weights = generate_pos_nums n 10 in
   let prices = generate_pos_nums n 10 in
   list_zip3 names weights prices |> list_to_array   

let%test "Test two implementations against each other" =  
  let max_weight = 30 in
  for _ = 1 to 20 do 
    let basket = generate_basket 10 in
    let res1 = knapsack_max_price max_weight basket in 
    let res2 = knapsack_max_price_dynamic max_weight basket |> fst in 
    assert (res1 = res2)
  done;
  true
   
(****************************************)
(*         Obtaining all items          *)
(****************************************)

let knapsack_obtain_items max_weight items =
  let num_items = Array.length items in 
  let (_, m) = knapsack_max_price_dynamic max_weight items in
  let res = ref [] in
  let w = ref max_weight in 
  for i = num_items downto 1 do
    if m.(i).(!w) = m.(i - 1).(!w) then ()
    else begin
      w := !w - weight items (i - 1);
      res :=  (i - 1) :: !res
    end
  done;
  !res

  