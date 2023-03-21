use "sol1.sml";
use "sol2.sml";
use "sol3.sml";
use "sol4.sml";
use "sol5.sml";

(*-- test sol1 --*)
"*** test sol1.sml ***";
val test1 = (merge([], []) = []);
val test2 = (merge([], [1,2,3]) = [1,2,3]);
val test3 = (merge([1,2,3], []) = [1,2,3]);
val test4 = (merge([1,2,3], [4,5,6]) = [1,2,3,4,5,6]);
val test5 = (merge([1,3,5], [2,4,6]) = [1,2,3,4,5,6]);

val tests = [test1, test2, test3, test4, test5];

(*-- test sol2 --*)
"*** test sol2.sml ***";
val test1 = (reverse [] = []);
val test2 = (reverse [1,2,3] = [3,2,1]);
val test3 = (reverse [1,2,3,4] = [4,3,2,1]);
val test4 = (reverse [1,2,3,4,5] = [5,4,3,2,1]);

val tests = [test1, test2, test3, test4];


(*-- test sol3 --*)
"*** test sol3.sml ***";
val test1 = (pi(1, 4, fn x => x) = 24);
val test2 = (pi(2, 5, fn x => x + 1) = 360);
val test3 = (pi(0, 3, fn x => x * x) = 0);
val test4 = (pi(1, 1, fn x => x) = 1);

val tests = [test1, test2, test3, test4];


(*-- test sol4 --*)
"*** test sol4.sml ***";
val test1 = (digits 0 = []);
val test2 = (digits 123 = [1,2,3]);
val test3 = (digits 4567 = [4,5,6,7]);
val test4 = (digits 100 = [1,0,0]);

val tests = [test1, test2, test3, test4];


(*-- test sol5 --*)
"*** test sol5.sml ***";
val test1 = (digitalRoot 0 = 0);
val test2 = (digitalRoot 5 = 5);
val test3 = (digitalRoot 12345 = 6);
val test4 = (digitalRoot 987654321 = 9);

val test5 = (additivePersistence 0 = 0);
val test6 = (additivePersistence 5 = 0);
val test7 = (additivePersistence 12345 = 2);
val test8 = (additivePersistence 987654321 = 2);

val tests = [test1, test2, test3, test4, test5, test6, test7, test8];
