zero? : Int -> Bool
zero? n = n == 0
;
pred : Int -> Int
pred n = n - 1
;
even? : Int -> Bool
even? n = if zero? n
          then True
          else odd? (pred n)
;
odd? : Int -> Bool
odd? n = if zero? n
         then False
         else even? (pred n)
;
even? 11
