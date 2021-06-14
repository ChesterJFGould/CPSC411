let x = if not 1 < 2
        then 10
        else 24
in let y = x * 3
in if x < y
   then let z = x * 10 in z + -2
   else y
