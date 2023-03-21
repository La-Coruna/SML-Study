(* n is positive integer *)
fun digitsSum(num: int):int =
	if num = 0
	then 0
	else num mod 10 + digitsSum(num div 10)
;

fun digitalRoot(n: int):int=
	if n < 10
	then n
	else digitalRoot(digitsSum(n))
;

fun additivePersistence(n: int):int=
	if n < 10
	then 0
	else additivePersistence(digitsSum (n)) + 1
;