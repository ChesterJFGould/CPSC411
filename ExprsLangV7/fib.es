one? n = n == 1
;
two? n = n == 2
;
fib n = if one? n
        then 0
        else if two? n
        then 1
        else fib (n - 1) * fib (n - 2)
;
fib 11
