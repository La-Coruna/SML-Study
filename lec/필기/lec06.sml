fun filter(f, xs) = 
  case xs of
    [] => [] 
  | x::xs' => if f x then x::filter(f, xs') 
                     else filter(f, xs')

(* returns true if for all element e in alist
 *    test(e) = true
 *)
fun ifall (test, alist) = 
  case alist of
    [] => true
  | x::xs => test(x) andalso ifall(test,xs)    

fun ifall(test, alist) =
  filter(test,alist) = alist

(* returns true if for any element e in alist
 *    test(e) = true
 *)
fun ifany (test, alist) = 
  case alist of
    [] => false
  | x::xs => test(x) orelse ifany(test, xs)

fun ifany(test, alist) =
  not(null(filter(test,alist)))




(* fun ifall (test, alist) = 
    case alist of
        [] => true
      | a::[] => test(a) 
      | a::alist' => test(a) andalso ifall(test, alist')

(* returns true if for any element e in alist
 *    test(e) = true
 *)
fun ifany (test, alist) = 
    case alist of
        [] => true
      | a::[] => test(a) 
      | a::alist' => test(a) orelse ifany(test, alist') *)