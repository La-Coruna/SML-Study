fun pi (a: int, b: int, f: int -> int) : int = 
  if a > b
  then 1
  else f(a)*pi(a+1,b,f)
;