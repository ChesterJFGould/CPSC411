let a = 1
  | b = 2
  in let c = if a < b then if a > b then 5 else 10 else 20
       | d = 3
       in c + d
