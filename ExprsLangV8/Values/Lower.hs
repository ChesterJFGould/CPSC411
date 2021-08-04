module Values.Lower
( lower
)
where

import Compiler.Gensym
import Compiler.Locs
import Compiler.Types
import qualified Monadic.Types as M
import Values.Types

lower :: Program -> Gensym M.Program
lower (Program defs body) = M.Program <$> lowerDefs defs
                                      <*> lowerBody [] body

lowerDefs :: [Func] -> Gensym [M.Block]
lowerDefs = mapM lowerFunc

lowerFunc :: Func -> Gensym M.Block
lowerFunc (Func name args body) = M.Block name <$> lowerBody args body

lowerBody :: [Aloc] -> Body -> Gensym M.Body
lowerBody args (Body expr) = do
                        lr <- genAloc "lr"
                        let lr' = MAloc lr
                            linkRegister' = (M.Triv . MMloc . MRloc . Reg) linkRegister
                            argStmts = zipWith M.Set
                                               (map MAloc args)
                                               (map (M.Triv . MMloc . MRloc) callLocations)
                        return (M.Body (M.TSeq [ M.Set lr' linkRegister' ]
                                               (M.TSeq argStmts (lowerTail lr' expr))))

lowerTail :: Mloc -> Expr -> M.Tail
lowerTail lr expr@(Triv _) = lowerReturn lr (lowerExpr expr)
lowerTail lr expr@(BinOp _ _ _) = lowerReturn lr (lowerExpr expr)
lowerTail lr expr@(MRef _ _) = lowerReturn lr (lowerExpr expr)
lowerTail lr expr@(Alloc _) = lowerReturn lr (lowerExpr expr)
lowerTail lr (Call f args) = M.TSeq argStmts
                                    (M.TSeq [M.Set ((MRloc . Reg) linkRegister) (M.Triv (MMloc lr)) ]
                                            (M.Jump (MLabel f) undeadOut))
                           where (argStmts, undeadOut) = lowerArgs args
lowerTail lr (Let assignments expr) = M.TSeq (lowerAssignments assignments)
                                             (lowerTail lr expr)
lowerTail lr (If p c a) = M.TIf (lowerPred p)
                                (lowerTail lr c)
                                (lowerTail lr a)
lowerTail lr (Seq stmts expr) = M.TSeq (lowerStmts stmts) (lowerTail lr expr)

lowerStmts :: [Stmt] -> [M.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [M.Stmt]
lowerStmt (MSet ptr offset expr) = [ M.MSet (lowerTriv ptr)
                                            (lowerTriv offset)
                                            (lowerExpr expr) ]
lowerStmt (SLet assignments stmt) = lowerAssignments assignments ++ lowerStmt stmt

lowerExpr :: Expr -> M.Expr
lowerExpr (Triv triv) = M.Triv (lowerTriv triv)
lowerExpr (BinOp op a b) = M.BinOp op (lowerTriv a) (lowerTriv b)
lowerExpr (MRef ptr offset) = M.MRef (lowerTriv ptr) (lowerTriv offset)
lowerExpr (Alloc size) = M.Alloc (lowerTriv size)
lowerExpr (Call f args) = M.Seq argStmts
                                (M.Seq [ M.JumpRet f undeadOut ]
                                       ((M.Triv . MMloc . MRloc . Reg) returnRegister))
                        where (argStmts, undeadOut) = lowerArgs args
lowerExpr (Let assignments expr) = M.Seq (lowerAssignments assignments)
                                         (lowerExpr expr)
lowerExpr (If p c a) = M.If (lowerPred p)
                            (lowerExpr c)
                            (lowerExpr a)
lowerExpr (Seq stmts expr) = M.Seq (lowerStmts stmts)
                                   (lowerExpr expr)

lowerPred :: Pred -> M.Pred
lowerPred (Bool b) = M.Bool b
lowerPred (RelOp op a b) = M.RelOp op (lowerTriv a) (lowerTriv b)
lowerPred (Not pred) = M.Not (lowerPred pred)
lowerPred (PLet assignments pred) = M.PSeq (lowerAssignments assignments)
                                           (lowerPred pred)
lowerPred (PIf p c a) = M.PIf (lowerPred p)
                              (lowerPred c)
                              (lowerPred a)
lowerPred (PSeq stmts pred) = M.PSeq (lowerStmts stmts)
                                     (lowerPred pred)

lowerTriv :: ATriv -> MTriv
lowerTriv (AAloc aloc) = MMloc (MAloc aloc)
lowerTriv (ALit lit) = MLit lit

lowerAssignments :: [(Aloc, Expr)] -> [M.Stmt]
lowerAssignments = map lowerAssignment

lowerAssignment :: (Aloc, Expr) -> M.Stmt
lowerAssignment (aloc, expr) = M.Set (MAloc aloc) (lowerExpr expr)

lowerReturn :: Mloc -> M.Expr -> M.Tail
lowerReturn lr expr = M.TSeq [ M.Set returnRegister' expr ]
                             (M.Jump (PMloc lr) [returnRegister', lr])
                    where returnRegister' = (MRloc . Reg) returnRegister

lowerArgs :: [ATriv] -> ([M.Stmt], [Mloc])
lowerArgs args = (argStmts, undeadOut)
               where argStmts = addrStmts ++ regStmts
                     addrStmts = zipWith M.Set
                                         (map (MRloc . Addr) callAddresses)
                                         (map (M.Triv . lowerTriv) (drop numRegs args))
                     regStmts = zipWith M.Set
                                        (map (MRloc . Reg) callRegisters)
                                        (map (M.Triv . lowerTriv) (take numRegs args))
                     numRegs = length callRegisters
                     undeadOut = map MRloc (take (length args) callLocations)
