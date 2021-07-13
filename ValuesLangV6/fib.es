fib n = if n < 2 then 0
        else if n == 2 then 1
        else let a = n - 1
               | b = n - 2
               in let a = fib a
                    | b = fib b
                    in a + b
;
fib 10
