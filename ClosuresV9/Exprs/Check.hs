module Exprs.Check
( check
)
where

import Exprs.DefCheck
import Exprs.TypeCheck
import Exprs.Types

check :: Program -> Either String Program
check = (>>= typeCheck)
        . defCheck
