applyTwice : (Int -> Int) -> Int -> Int
applyTwice f n = f (f n)
;
add : Int -> Int -> Int
add a b = a + b
;
add2 : Int -> Int
add2 = add 2
;
applyTwice add2 2
