fun findString(s:string, l:string list) =
  if null l
  then false
  else if s = (hd l)
    then true
    else findString(s, tl l)
;

(* (*test code*)
val a = ["ab","cd","ed","fg"];

findString("ab",a);
findString("fg",a);
findString("bc",a);
*)

fun RemoveElementInList(x, l) =
  case l of
    [] => []
  | y::ys => if x = y
              then RemoveElementInList (x, ys)
              else y::(RemoveElementInList(x, ys))
;

(* (*test code*)
RemoveElementInList("a",["a","b","a","c","a"]);

RemoveElementInList([1,2],[[1,2],[2,3]]);

*)

fun mergeList(x: 'a list, y: 'a list):'a list=
  if null x
  then y
  else hd x::(mergeList(tl x,y))
;
