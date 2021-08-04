eq? : Int -> Int -> Bool
eq? a b = a == b
;
one? : Int -> Bool
one? = eq? 1
;
two? : Int -> Bool
two? = eq? 2
;
fib : Int -> Int
fib n = if one? n
        then 0
        else if two? n
        then 1
        else fib (n - 1) + fib (n - 2)
;
fib 10
