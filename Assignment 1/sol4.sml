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