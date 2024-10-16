(* tuples p.15 *)

fun swap (pr : int*bool) =
  (#2 pr, #1 pr)
;

fun sum_two_pairs (pr1 : int*int, pr2 : int*int) =
  (#1 pr1) + (#1 pr2) + (#2 pr1) + (#2 pr2)
;

fun div_mod (x:int , y:int) =
  (x div y, x mod y)
;

fun sort_pair (pr: int * int) =
  if #1 pr < #2 pr
  then pr
  else (#2 pr, #1 pr)
;

(* lists p.24 *)

fun sum_list (xs : int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs)
;

fun countdown (x : int) =
  if x = 0
  then []
  else x :: countdown(x-1)
;

(* [] [1 2 3 4 5 6]
[1] [2 3 4 5 6]
[1] :: [2] [3 4 5 6]
[1] :: [2] :: [3] [4 5 6] *)

fun append (xs : int list, ys : int list) =
  if null xs
  then ys
  else hd xs :: append(tl xs, ys)
;

(* ver 1 *)
fun countup (x:int) =
  let val y = x+1;
      fun count (x:int) =
        if x = 0
        then []
        else (y-x) :: count(x-1)
      ;
  in
    count(x)
  end
;

(* ver 2 *)
fun countup (x:int) =
  let fun count(x, ys) =
        if x = 0
        then ys
        else count(x-1,x::ys);
  in
    count(x,[])
  end
;

fun sum_pair_list (xs : (int*int) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)
;

fun firsts (xs : (int*int) list) =
  if null xs
  then []
  else  #1 (hd xs) :: firsts(tl xs)
;

fun seconds (xs : (int*int) list) =
  if null xs
  then []
  else #2 (hd xs) :: seconds(tl xs)
;

fun sum_pair_list2 (xs : (int*int) list) =
  sum_list(firsts(xs)) + sum_list(seconds(xs))
;

(* silly p.31 *)

fun silly1 (z : int) =
  let val x = if z > 0 then z else 34
      val y = x+z+9
  in
    if x > y then x*2 else y*y
  end

fun silly2 () =
  let val x = 1
  in
    (let val x = 2 in x+1 end) +
    (let val y = x+2 in y+1 end)
  end

(* inferior countup p.34 *)
fun countup_from1(x: int) =
  let fun count(from:int, to:int) =
        if from = to
        then from :: []
        else from :: count(from+1,to)
  in
    count(1,x)
  end
;

(* better countup p.34 *)
fun countup_from1(x: int) =
  let fun count(from:int) =
        if from = x
        then from :: []
        else from :: count(from+1)
  in
    count(1)
  end
;
(* Unnecessary parameters are usually bad style
– Like to in previous example *)

(* better max p.44 *)
fun better_max (xs : int list) =
  if null xs
  then NONE
  else
    let val tl_ans = better_max(tl xs)
    in
      if isSome tl_ans
        andalso valOf tl_ans > hd xs
      then tl_ans
      else SOME (hd xs)
    end