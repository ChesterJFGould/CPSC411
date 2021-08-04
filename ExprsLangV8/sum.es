sum lst = if empty? lst
          then 0
          else car lst + sum (cdr lst)
;
sum (1 :: 2 :: 3 :: 4 :: 5 :: [])
