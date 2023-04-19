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