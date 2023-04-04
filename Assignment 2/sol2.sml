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
  if null (checkMetroHelp(m))
  then true
  else false
;

(* soultion #3 *)
datatype 'a lazyList =
    nullList
  | cons of 'a * (unit -> 'a lazyList)

fun seq(first,last):int lazyList =
  if first > last
  then nullList
  else
    let 
      fun seqHelp() = seq(first+1,last)
    in
      cons(first, seqHelp)
    end
;

fun infSeq(first):int lazyList =
  let
    fun infSeqHelp() = infSeq(first+1)
  in  
    cons(first, infSeqHelp)
  end
;

fun firstN(lazyListVal: 'a lazyList ,n :int):'a list =
  case (lazyListVal,n) of
      (nullList,_) => []
    | (_,0) => []
    | (cons(first,next),n) => first :: firstN(next(),n-1)
;

fun Nth(lazyListVal: 'a lazyList ,n :int):'a option =
  case (lazyListVal,n) of
      (nullList,_) => NONE
    | (_,0) => NONE
    | (cons(first,next),1) => SOME first
    | (cons(first,next),n) => Nth(next(),n-1)
; 
fun filterMultiples(lazyListVal,n)=
  case lazyListVal of
      nullList => nullList
    | cons(first,getNext) =>
        let
          fun skip() = filterMultiples(getNext(),n)
        in
          if first mod n = 0
          then filterMultiples(getNext(),n)
          else cons(first,skip)
        end
;


fun primes()=
  let
    fun sieve(lazyListVal)=
      case lazyListVal of
          nullList => nullList
        | cons(first,getNext) =>
            let
              fun sieveHelp() = sieve(filterMultiples(lazyListVal,first))
            in
              cons(first,sieveHelp)
            end
  in
    sieve(infSeq(2))
  end
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


(*test 2*)
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

(*test3*)
val seqAndfirstNTest1 = firstN(seq(1, 5), 3) = [1, 2, 3];
val seqAndfirstNTest2 = firstN(seq(~5, 5), 3) = [~5, ~4, ~3];
val seqAndfirstNTest3 = firstN(seq(1, 5), 10) = [1, 2, 3, 4, 5];
val seqAndfirstNTest4 = firstN(seq(5, 5), 3) = [5];
val seqAndfirstNTest5 = firstN(seq(5, 1), 3) = [];
val seqAndfirstNTest6 = firstN(seq(1, 5), 0) = [];

val seqAndNthTest1 = Nth(seq(1, 5), 3) = SOME 3;
val seqAndNthTest2 = Nth(seq(~5, 5), 3) = SOME ~3;
val seqAndNthTest3 = Nth(seq(5, 5), 3) = NONE;
val seqAndNthTest4 = Nth(seq(5, 1), 3) = NONE;
val seqAndNthTest5 = Nth(seq(1, 5), 0) = NONE;

val infSeqAndfisrtNTest1 = firstN(infSeq(1), 3) = [1, 2, 3];
val infSeqAndfisrtNTest2 = firstN(infSeq(1), 10) = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
val infSeqAndfisrtNTest3 = firstN(infSeq(1), 0) = [];
val infSeqAndfisrtNTest4 = firstN(infSeq(~10), 3) = [~10, ~9, ~8];

val infSeqAndNthTest1 = Nth(infSeq(1), 3) = SOME 3;
val infSeqAndNthTest2 = Nth(infSeq(1), 0) = NONE;
val infSeqAndNthTest3 = Nth(infSeq(1), 1073741823) = SOME 1073741823;
val infSeqAndNthTest4 = Nth(infSeq(~100), 3) = SOME ~98;

val filterMultiplesTest1 = firstN(filterMultiples(seq(1, 5), 3), 10) = [1, 2, 4, 5];
val filterMultiplesTest2 = firstN(filterMultiples(seq(1, 20), 2), 5) = [1, 3, 5, 7, 9];
val filterMultiplesTest3 = firstN(filterMultiples(seq(~5, 5), 3), 5) = [~5, ~4, ~2, ~1, 1];
val filterMultiplesTest4 = firstN(filterMultiples(seq(~5, 5), 1), 5) = [];
val filterMultiplesTest5 = firstN(filterMultiples(seq(5, 1), 5), 5) = [];
 *)
val primesTest1 = firstN(primes(), 10) = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29];
val primesTest2 = Nth(primes(), 20) = SOME 71;