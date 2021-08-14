makePair : Int -> Int -> (Int -> Int -> Int) -> Int
makePair a b handler = handler a b
;
fst : Int -> Int -> Int
fst a b = a
;
snd : Int -> Int -> Int
snd a b = b
;
(makePair 10 20) snd
