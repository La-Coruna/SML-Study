(* 2019019043 박종윤 *)
(* Assignment3 *)

datatype pattern = Wildcard | Variable of string | UnitP
	| ConstP of int | TupleP of pattern list
	| ConstructorP of string * pattern
datatype valu = Const of int | Unit | Tuple of valu list
	| Constructor of string * valu

(* 1. check_pat *)
fun check_pat(p: pattern):bool =
	let
		fun makeStrList(p: pattern, sl: string list): string list = 
			case p of
					Variable(s) => s :: sl
				|	TupleP(pl) => List.foldl makeStrList sl pl
				| ConstructorP(s,p2) => makeStrList(p2, sl)
				| _ => sl		(* Wildcard, UnitP, ConstP *)
		fun isDistinct(sl: string list): bool =
			case sl of
					[] => true
				|	s::[] =>true
				|	s::sl' => (not(List.exists (fn x => x=s) sl')) andalso isDistinct(sl')
	in
		isDistinct(makeStrList(p,[]))
	end

(* 2. match *)
fun match(v:valu, p:pattern):(string * valu) list option =
	case (v,p) of
			(_,Wildcard) => SOME []
		|	(_,Variable(s)) => SOME [(s,v)]
		|	(Unit,UnitP) => SOME []
		|	(Const(vi),ConstP(pi)) => if(vi =pi) then SOME[] else NONE
		| (Tuple(vs), TupleP(ps)) =>
				if ((List.length vs) = (List.length ps))
				then
					let
						fun isMatch(vs,ps):bool =
							case (vs,ps) of
									([],[]) => true
								|	(v'::vs',p'::ps') => isSome(match(v',p')) andalso isMatch(vs',ps')
					in
						if(isMatch(vs,ps)) then SOME( List.foldl (fn(x,acc)=> valOf(match(x)) @ acc) [] (ListPair.zip(vs,ps))) else NONE
					end
				else NONE
		| (Constructor(s1,v'),ConstructorP(s2,p')) => if(s1=s2) then match(v',p') else NONE
		| _ => NONE

(* 3. Rock, Paper, Scissors *)
type name = string
datatype RSP = 
		ROCK
	| SCISSORS
	| PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament =
		PLAYER of name * (RSP strategy ref)
	| MATCH of tournament * tournament

fun onlyOne(one: RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one: RSP, two: RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one: RSP, two: RSP, three: RSP) = Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)
val rps = alterThree(ROCK, PAPER, SCISSORS)

fun next(strategyRef) =
	let
		val Cons(rsp, func) = !strategyRef
	in
		strategyRef := func();
		rsp
	end

fun whosWinner(t) = 
	let
		fun fight(p1, p2) =
			case (p1,p2) of
				(PLAYER(_,s1),PLAYER(_,s2)) =>
					case (!s1,!s2) of
							(Cons(ROCK,_), Cons(SCISSORS,_)) => (next(s1);next(s2);p1)
						|	(Cons(SCISSORS,_), Cons(PAPER,_)) => (next(s1);next(s2);p1)
						|	(Cons(PAPER,_), Cons(ROCK,_)) => (next(s1);next(s2);p1)
						|	(Cons(ROCK,_), Cons(PAPER,_)) => (next(s1);next(s2);p2)
						|	(Cons(SCISSORS,_), Cons(ROCK,_)) => (next(s1);next(s2);p2)
						|	(Cons(PAPER,_), Cons(SCISSORS,_)) => (next(s1);next(s2);p2)
						| _ => (next(s1);next(s2);fight(p1,p2))
	in
		case t of
				PLAYER(_) => t
			|	MATCH(t1, t2) => 
					case (t1, t2) of
							(PLAYER(_,s1),PLAYER(_,s2)) => fight(t1,t2) (* player vs player *)
						|	(PLAYER(_,_),MATCH(_,_)) => fight(t1,whosWinner(t2))  (* player vs tournament *) 
						|	(MATCH(_,_),PLAYER(_,_)) => fight(whosWinner(t1),t2)  (* tournament vs player *) 
						|	(MATCH(_,_),MATCH(_,_)) => fight(whosWinner(t1),whosWinner(t2))  (* tournament vs tournament *) 
	end
