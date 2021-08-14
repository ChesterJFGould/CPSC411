zero? : Int -> Bool
zero? n = n == 0
;
true? : Int -> Bool
true? n = True
;
pred : Int -> Int
pred n = n - 1
;
even? : Int -> Bool
even? n = if (zero? n)
          then True
          else odd? (n - 1)
;
odd? : Int -> Bool
odd? n = if (n == 0)
         then False
         else even? (n - 1)
;
if even? 10
then 1
else 0
