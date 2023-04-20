fun merge(xs: int list, ys: int list):int list =
  if null xs
  then ys
  else
    if null ys then xs else
      if hd xs < hd ys
      then hd xs :: merge(tl xs, ys)
      else hd ys :: merge(xs, tl ys)
;

fun reverse(xs:int list):int list =
	let
		fun reverse_help(org:int list,res:int list) :int list=
			if null org
			then res
			else reverse_help(tl org, (hd org) :: res);
	in
		reverse_help(xs,[])
	end
;

fun pi (a: int, b: int, f: int -> int) : int = 
  if a > b
  then 1
  else f(a)*pi(a+1,b,f)
;

fun digits (num : int) : int list =
  let
    fun digits_help(x:int, res:int list) : int list =
      if x = 0
      then res
      else digits_help (x div 10 ,(x mod 10) :: res)
  in
    digits_help(num,[])
  end
;

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