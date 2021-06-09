let x = 3
  | y = 20
  | z = 31
  in if let z = x + y in if z >= 23 then true else true
     then let x = x + y in x * 4
     else y + z
