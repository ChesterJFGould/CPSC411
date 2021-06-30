id a = a
;
add a b = let a = id a
            | b = id b
            in a + b
;
add 10 20
