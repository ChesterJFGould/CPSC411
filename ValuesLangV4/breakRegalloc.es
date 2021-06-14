let x = if 1 < 2
        then let y = 10
               | z = 2
               in z + y
        else 20
in let y = -10 
   in let z = x * y
      in z + x
