double n = addSwap n n
;
addSwap a b = add b a
;
add a b = let n = a + b
          in id n
;
id n = n
;
let a = 1
  | b = 2
  in if a > b
     then double a
     else double b
