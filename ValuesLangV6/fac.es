fac n = if n < 1
        then 1
        else let predN = pred n
             in let facPredN = fac predN
                in n * facPredN
;
pred n = let one = 1
         in n - one
;
fac 10
