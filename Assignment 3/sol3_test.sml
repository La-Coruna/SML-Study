(* 2019019043 박종윤 *)
(* Assignment3 *)
use "sol3.sml";

(*  *)
val p1 = Variable("a")
val p2 = ConstructorP("wildcard", Wildcard)
val p3 = ConstructorP("constructor", TupleP([ConstP(17), Variable("a"), TupleP([Variable("b"), Wildcard, Variable("c"), TupleP([UnitP, Variable("d"), Variable("e")])])]))
val p4 = ConstructorP("constructor", TupleP([ConstP(17), Variable("a"), TupleP([Variable("b"), Wildcard, Variable("c"), TupleP([UnitP, Variable("d"), Variable("a")])])]))
val p5 = ConstructorP("constructor", TupleP([ConstP(17), Variable("a"), TupleP([Variable("b"), Wildcard, Variable("c"), TupleP([UnitP, Variable("d"), Variable("e")])]), Variable("f")]))
val p6 = ConstructorP("constructor", TupleP([ConstP(17), Variable("a"), TupleP([Variable("b"), Wildcard, Variable("c"), TupleP([UnitP, Variable("d"), Variable("e")])]), Variable("f"), Variable("d")]))
val v1 = Const(42)
val v2 = Constructor("wild", Unit)
val v3 = Constructor("wildcard", Const(42))
val v4 = Constructor("constructor", Tuple([Const(17), Unit, Tuple([Const(18), Const(19), Const(20), Tuple([Unit, Const(21), Unit])])]))
val v5 = Constructor("constructor", Tuple([Const(17), Unit, Tuple([Const(18), Const(19), Const(20), Tuple([Unit, Const(21), Unit])]), Const(22)]))
val v6 = Constructor("constructor", Tuple([Const(15), Unit, Tuple([Const(18), Const(19), Const(20), Tuple([Unit, Const(21), Unit])]), Const(22)]))
val v7 = Constructor("constructor", Tuple([Const(17), Unit, Tuple([Const(19), Const(20), Tuple([Unit, Const(21), Unit])]), Const(22)]))
val winner1 = PLAYER("rp", ref rp)
val winner2 = PLAYER("Emily", ref srp)
val winner3 = PLAYER("rps", ref rps)

val checkPatTest1 = check_pat(p1) = true
val checkPatTest2 = check_pat(p2) = true
val checkPatTest3 = check_pat(p3) = true
val checkPatTest4 = check_pat(p4) = false
val checkPatTest5 = check_pat(p5) = true
val checkPatTest6 = check_pat(p6) = false

val matchTest1 = match(v1, p1) = SOME [("a", Const(42))] 
val matchTest2 = match(v2, p2) = NONE
val matchTest3 = match(v3, p2) = SOME []
val matchTest4 = match(v4, p3) = SOME [("e", Unit), ("d", Const(21)), ("c", Const(20)), ("b", Const(18)), ("a", Unit)]
val matchTest5 = match(v5, p5) = SOME [("f", Const(22)), ("e", Unit), ("d", Const(21)), ("c", Const(20)), ("b", Const(18)), ("a", Unit)]
val matchTest6 = match(v6, p5) = NONE
val matchTest7 = match(v7, p5) = NONE
val matchTest8 = match(Tuple [], TupleP []) = SOME []

val whosWinnerTest1 = whosWinner(MATCH(PLAYER("s", ref s), MATCH(winner1, PLAYER("r", ref r)))) = winner1
val whosWinnerTest2 = whosWinner(MATCH(MATCH(PLAYER("John", ref sr), PLAYER("Steve", ref s)), MATCH(PLAYER("Alice", ref p), MATCH(PLAYER("David", ref r), MATCH(PLAYER("Bill", ref s), winner2))))) = winner2
val whosWinnerTest3 = whosWinner(MATCH(PLAYER("s", ref s), MATCH(winner3, PLAYER("r", ref r)))) = winner3
(*  *)