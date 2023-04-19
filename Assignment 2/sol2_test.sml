use "sol2.sml";

(*test 1*)
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

val primesTest1 = firstN(primes(), 10) = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29];
val primesTest2 = Nth(primes(), 20) = SOME 71;