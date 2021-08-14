succ : Int -> Int
succ n = if succ' n == 2
         then 2
         else n
;
succ' : Int -> Int
succ' n = n + 1
;
let x = 10
in if succ 1 == 2
   then 1
   else 0
