module Para.Types where

data Program = Program [Label] [Stmt]

data Stmt = Set Rloc RTriv
          | BinOp BinOp Rloc RTriv
          | MRef Rloc RTriv RTriv
          | MSet RTriv RTriv RTriv
          | Compare Rloc RTriv
          | JumpIf RelOp
          | Jump RTriv
          | Labelled Label Stmt
