fac n acc = if n < 1
            then acc
            else fac (n - 1) (n * acc)
;
fac 10 1
