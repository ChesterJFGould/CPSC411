fac n = fac' n 1

fac' n acc = if n < 1
             then acc
             else let acc' = acc * n
                    | n' = n - 1
                    in fac' n' acc'

fac 10
