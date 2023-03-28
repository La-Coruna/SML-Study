(* 2019019043 박종윤 *)

(* solution #1 *)

datatype expr =
    NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
datatype formula = 
    TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
  (* LESS(a, b) is true if a < b *)

fun eval(f: formula):bool =
  case f of
      TRUE => true
    | FALSE => false
    | NOT f1 => not (eval f1)
    | ANDALSO (f1,f2) => (eval f1) andalso (eval f2)
    | ORELSE (f1,f2) => (eval f1) orelse (eval f2)
    | IMPLY (f1,f2) => (not(eval f1)) orelse (eval f2)
    | LESS (e1, e2) =>  
        let
          fun eval_expr(e: expr):int =
            case e of
                NUM e => e
              | PLUS(e1, e2) => (eval_expr e1) + (eval_expr e2)
              | MINUS(e1, e2) => (eval_expr e1) - (eval_expr e2)
        in
          eval_expr(e1) < eval_expr(e2)
        end
;

(* solution #2 *)

type name = string
datatype metro =
    STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro

(* help function *)
fun RemoveElementInList(x, l) =
  case l of
    [] => []
  | y::ys => if x = y
              then RemoveElementInList (x, ys)
              else y::(RemoveElementInList(x, ys))
;
fun mergeList(x: 'a list, y: 'a list):'a list=
  if null x
  then y
  else hd x::(mergeList(tl x,y))
;
fun checkMetroHelp(m:metro):string list =
  case m of
      STATION name => [name]
    | AREA (name, metro) => RemoveElementInList(name, checkMetroHelp(metro))
    | CONNECT (metro1, metro2) => mergeList(checkMetroHelp(metro1), checkMetroHelp(metro2))
;

(* solution function *)
fun checkMetro(m: metro):bool =
  if null checkMetroHelp(m)
  then true
  else false
;




(* (*test 1*)
val evalTest1 = eval(TRUE) = true;
val evalTest2 = eval(FALSE) = false;
val evalTest3 = eval(NOT(TRUE)) = false;
val evalTest4 = eval(NOT(FALSE)) = true;
val evalTest5 = eval(ANDALSO(TRUE, TRUE)) = true;
val evalTest6 = eval(ANDALSO(TRUE, FALSE)) = false;
val evalTest7 = eval(ANDALSO(FALSE, TRUE)) = false;
val evalTest8 = eval(ANDALSO(FALSE, FALSE)) = false;
val evalTest9 = eval(ORELSE(TRUE, TRUE)) = true;
val evalTest10 = eval(ORELSE(TRUE, FALSE)) = true;
val evalTest11 = eval(ORELSE(FALSE, TRUE)) = true;
val evalTest12 = eval(ORELSE(FALSE, FALSE)) = false;
val evalTest13 = eval(IMPLY(TRUE, TRUE)) = true;
val evalTest14 = eval(IMPLY(TRUE, FALSE)) = false;
val evalTest15 = eval(IMPLY(FALSE, TRUE)) = true;
val evalTest16 = eval(IMPLY(FALSE, FALSE)) = true;
val evalTest17 = eval(LESS(NUM(1), NUM(2))) = true;
val evalTest18 = eval(LESS(PLUS(NUM(1), NUM(2)), NUM(3))) = false;
val evalTest19 = eval(LESS(MINUS(NUM(1), NUM(2)), NUM(3))) = true;
val evalTest20 = eval(ANDALSO(LESS(MINUS(NUM(1), NUM(2)), NUM(3)), LESS(NUM(5), PLUS(NUM(42), NUM(15))))) = true;
*)

(* (*test 2*))
val checkMetroTest1 = checkMetro(AREA("a", STATION "a")) = true;
val checkMetroTest2 = checkMetro(AREA("a", AREA("a", STATION "a"))) = true;
val checkMetroTest3 = checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) = true;
val checkMetroTest4 = checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) = true;
val checkMetroTest5 = checkMetro(AREA("a", STATION "b")) = false;
val checkMetroTest6 = checkMetro(AREA("a", AREA("a", STATION "b"))) = false;
val checkMetroTest7 = checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) = false;
val checkMetroTest8 = checkMetro(AREA("a", CONNECT(STATION "b", AREA("b", STATION "a")))) = false;
val checkMetroTest9 = checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) = false;
val checkMetroTest10 = checkMetro(STATION("a")) = false;
val checkMetroTest11 = checkMetro(CONNECT(STATION("a"), AREA("b", STATION "b"))) = false;
val checkMetroTest12 = checkMetro(CONNECT(AREA("a", STATION("a")), AREA("b", STATION "b"))) = true;
val checkMetroTest13 = checkMetro(CONNECT(AREA("a", STATION("a")), AREA("b", STATION "a"))) = false;
*)