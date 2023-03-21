fun merge(xs: int list, ys: int list):int list =
  if null xs
  then ys
  else
    if null ys then xs else
      if hd xs < hd ys
      then hd xs :: merge(tl xs, ys)
      else hd ys :: merge(xs, tl ys)
;
