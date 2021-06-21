fac n = facPrime n 1
;
facPrime n acc = if n < 1
                 then if true then acc else acc
                 else let accPrime = acc * n
                        | nPrime = n + -1
                        in facPrime nPrime accPrime
;
fac 12
