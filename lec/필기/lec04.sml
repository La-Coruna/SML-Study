datatype exp = Constant of int
             | Negate   of exp
             | Add      of exp*exp
             | Multiply of exp*exp

fun eval e =
  case e of
    Constant(i) => i
  | Negate(e) => ~(eval e)
  | Add(e1,e2) => eval(e1) + eval(e2)
  | Multiply(e1, e2) => (eval e1) * (eval e2)

fun eval (Constant(i)) = i
  | eval (Negate(e)) = ~ (eval e)
  | eval (Add(e1,e2)) = (eval e1) + (eval e2)
  | eval (Multiply(e1,e2)) = (eval e1) * (eval e2)

fun fibo 0 = 1
  | fibo 1 = 1
  | fibo n = fibo(n-1) + fibo(n-2)

fun fibo_series 0 = [fibo(0)]
  | fibo_series n = fibo_series(n-1) @ [fibo(n)]

(* 빠른 버전 p.7 *)
fun fibo 0 = (1, 1)
  | fibo n =
    let val (n_1, n_2) = fibo(n-1)
    in
      (n_1+n_2, n_1)
    end

(* p.8 *)
exception ListLengthMismatch;
fun zip3 lists =
  case lists of
    ([],[],[]) => []
  | (x::xs, y::ys, z::zs) => [x,y,z] :: zip3(xs,ys,zs)
  | _ => raise ListLengthMismatch
(*
* ([1,2,3], [10,20,30], [100,200,300])
* => [(1,10,100), (2,20,200), (3,30,300)]
*)

fun unzip3 triples =
  case triples of
    [] => ([],[],[])
  | (a,b,c)::tl => 
      let val (l1,l2,l3) = unzip3(tl)
      in
        (a::l1,b::l2,c::l3)
      end

(*
* [(1,10,100), (2,20,200), (3,30,300)]
* =>([1,2,3], [10,20,30], [100,200,300])
*)


(* p.11 *)
fun nondecreasing xs =
  case xs of
    [] => true
  | x::[] => true
  | x1::x2::xs => (x1 <= x2) andalso nondecreasing(x2::xs)
(* returns true if the list is nondecreasing.
* [1,2,3,4] => true
* [3,2,1] => false
*)

datatype sgn = P | N | Z

fun multsign (x1, x2) =
  case (x1, x2) of
    (P,P) => P
  | (N,P) => N
  | (P,N) => N
  | (N,N) => P
  | (_,_) => Z


(* returns the sign of multiplying x1 and x2.
* P for positive,
* N for negative,
* Z for zero
* multsign(0, 1) => Z, multsign(~1, 1) => N
*)

fun len(l: 'a list) =
    case l of
         [] => 0
       | _::xs => 1+len(xs)


(* exception 실험 *)
exception MyFirstException;

fun divide(a: int, b: int): int =
  if b = 0 then raise MyFirstException
  else a div b

val result = divide(10, 0) handle MyFirstException => 2

(* factorial p.20 *)
fun factorial(n) =
  n*factorial(n-1)


fun factorial (n) =
  let
    fun help(n,acc) = 
      if n = 1
      then acc
      else help(n-1,acc*n)
  in
    help(n,1)
  end

(* [1,2,3] = 1::[2,3] = 1::2::[3] *)
fun sum xs =
  case xs of
    [] => 0
    | x::xs' => x + sum xs'

fun sum xs=
  let
    fun help(xs,acc) =
      case xs of
        [] => acc
      | x::xs' => help(xs',acc+x)
  in
    help(xs,0)
  end

fun sum xs =
  let fun aux(xs,acc) =
    case xs of
      [] => acc
    | x::xs' => aux(xs',x+acc)
  in
    aux(xs,0)
  end

fun rev xs =
  case xs of
    [] => []
  | x::xs' => (rev xs') @ [x]

fun merge(xs, ys)=
  case (xs,ys) of
    ([],ys) => ys
  | (x::xs',ys) => x::merge(xs',ys)

(* [] => []
[1] => [1] = 1::[]
[1,2] => [2,1] = 2::[1] = 2::1::[]
[1,2,3] => [3,2,1] = 3::[2,1] = 3::2::[1] = 3::2::1::[] *)


fun rev xs=
  let
    fun aux(xs, acc) =
      case xs of
        [] => acc
      | x::xs' => aux(xs', x::acc)
  in
    aux(xs,[])
  end

fun n_times (f,n,x) =
	if n = 1
	then f(x)
	else n_times(f,n-1,f(x))

fun n_times (f,n,x) =
  if n=0
  then x
  else  n_times(f,n-1,f(x))

fun n_times (f,n,x) =
  if n=0
  then x
  else  f(n_times(f,n-1,x))

fun double x = x + x
fun increment x = x + 1
val x1 = n_times(double,4,7) (* 7*16 *)
val x2 = n_times(increment,4,7) (* 11 *)