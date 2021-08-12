module Values.Lower
( lower
)
where

import Compiler.Gensym
import qualified Compiler.Locations as Locations
import Compiler.Types
import qualified Monadic.Types as M
import Values.Types

import Control.Monad.Reader

type Link = ReaderT Aloc (Gensym)

lower :: Program -> Gensym M.Program
lower (Program labels funcs body) = M.Program labels <$> lowerFuncs funcs
                                                     <*> lowerBody body

lowerFuncs :: [Func] -> Gensym [M.Block]
lowerFuncs = mapM lowerFunc

lowerFunc :: Func -> Gensym M.Block
lowerFunc (Func label env arg body) = M.Block label <$> lowerBodyArgs env arg body

lowerBodyArgs :: Aloc -> Aloc -> Body -> Gensym M.Body
lowerBodyArgs env arg (Body bodyExpr) = do
                                        let bodyExprLinkComputation = lowerTail bodyExpr
                                        bodyExpr' <- genLinkAloc bodyExprLinkComputation
                                        let envMloc = MAloc env
                                            envRegisterExpr = regToExpr Locations.envRegister
                                            envLoadStmt = M.Set envMloc envRegisterExpr
                                        let argMloc = MAloc arg
                                            argRegisterExpr = regToExpr Locations.argRegister
                                            argLoadStmt = M.Set argMloc argRegisterExpr
                                        return ( M.Body ( M.TSeq [ envLoadStmt, argLoadStmt ]
                                                                 bodyExpr'
                                                        )
                                               )

lowerBody :: Body -> Gensym M.Body
lowerBody (Body expr) = M.Body <$> genLinkAloc (lowerTail expr)

genLinkAloc :: Link M.Tail -> Gensym M.Tail
genLinkAloc linkComputation = do
                              linkAloc <- genAloc "linkAloc"
                              tail <- runReaderT linkComputation linkAloc
                              let linkMloc = MAloc linkAloc
                                  linkRegisterExpr = regToExpr Locations.linkRegister
                                  linkLoadStmt = M.Set linkMloc linkRegisterExpr
                              return ( M.TSeq [ linkLoadStmt ]
                                              tail
                                     )

lowerTail :: Expr -> Link M.Tail
lowerTail (Apply f env arg) = do
                              let (used, argLoadStmts) = lowerCallArgs env arg
                              linkAloc <- ask
                              let linkRegisterMloc = (MRloc . Reg) Locations.linkRegister
                                  linkAlocExpr = alocToExpr linkAloc
                                  linkRegisterLoadStmt = M.Set linkRegisterMloc linkAlocExpr
                              let tailCall = M.Jump (lowerTriv f) used
                              return ( M.TSeq argLoadStmts
                                              ( M.TSeq [ linkRegisterLoadStmt ]
                                                       tailCall
                                              )
                                     )
lowerTail (Let var val body) = lowerLet M.TSeq var val <$> lowerTail body
lowerTail (If p c a) = M.TIf (lowerPred p) <$> lowerTail c
                                           <*> lowerTail a
lowerTail (Seq stmts expr) = M.TSeq (lowerStmts stmts) <$> lowerTail expr
lowerTail expr = do
                 let returnRegisterMloc = (MRloc . Reg) Locations.returnRegister
                     expr' = lowerExpr expr
                     returnRegisterLoadStmt = M.Set returnRegisterMloc expr'
                 linkAloc <- ask
                 let linkMloc = MAloc linkAloc
                     linkTriv = MMloc linkMloc
                 return ( M.TSeq [ returnRegisterLoadStmt ]
                                 ( M.Jump linkTriv [ returnRegisterMloc
                                                   , linkMloc
                                                   ]
                                 )
                        )

lowerExpr :: Expr -> M.Expr
lowerExpr (Triv triv) = M.Triv (lowerTriv triv)
lowerExpr (NumOp op a b) = M.NumOp op (lowerTriv a)
                                      (lowerTriv b)
lowerExpr (Apply f env arg) = let (used, argLoadStmts) = lowerCallArgs env arg
                                  callStmt = M.JumpRet (lowerTriv f) used
                                  returnRegisterExpr = regToExpr Locations.returnRegister
                              in M.Seq argLoadStmts
                                       ( M.Seq [ callStmt ]
                                               returnRegisterExpr
                                       )
lowerExpr (Let var val body) = lowerLet M.Seq var val (lowerExpr body)
lowerExpr (Alloc triv) = M.Alloc (lowerTriv triv)
lowerExpr (MRef ptr offset) = M.MRef (lowerTriv ptr)
                                     (lowerTriv offset)
lowerExpr (If p c a) = M.If (lowerPred p)
                            (lowerExpr c)
                            (lowerExpr a)
lowerExpr (Seq stmts expr) = M.Seq (lowerStmts stmts)
                                   (lowerExpr expr)

lowerStmts :: [Stmt] -> [M.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [M.Stmt]
lowerStmt (MSet ptr offset expr) = [ M.MSet (lowerTriv ptr)
                                            (lowerTriv offset)
                                            (lowerExpr expr)
                                   ]
lowerStmt (SLet var val stmt) = M.Set (MAloc var) (lowerExpr val) : lowerStmt stmt

lowerPred :: Pred -> M.Pred
lowerPred (Bool b) = M.Bool b
lowerPred (RelOp op a b) = M.RelOp op (lowerTriv a)
                                      (lowerTriv b)
lowerPred (Not pred) = M.Not (lowerPred pred)
lowerPred (PLet var val body) = lowerLet M.PSeq var val (lowerPred body)
lowerPred (PIf p c a) = M.PIf (lowerPred p)
                              (lowerPred c)
                              (lowerPred a)

lowerLet :: ([M.Stmt] -> a -> a) -> Aloc -> Expr -> a -> a
lowerLet seqCons var val body = let varMloc = MAloc var
                                    val' = lowerExpr val
                                    varSetStmt = M.Set varMloc val'
                                in seqCons [ varSetStmt ] body

lowerCallArgs :: ATriv -> ATriv -> ([Mloc], [M.Stmt])
lowerCallArgs env arg = let envExpr = M.Triv (lowerTriv env)
                            envRegisterMloc = (MRloc . Reg) Locations.envRegister
                            envRegisterLoadStmt = M.Set envRegisterMloc envExpr
                            argExpr = M.Triv (lowerTriv arg)
                            argRegisterMloc = (MRloc . Reg) Locations.argRegister
                            argRegisterLoadStmt = M.Set argRegisterMloc argExpr
                        in ( [ envRegisterMloc
                             , argRegisterMloc
                             ]
                           , [ envRegisterLoadStmt
                             , argRegisterLoadStmt
                             ]
                           )

lowerTriv :: ATriv -> MTriv
lowerTriv (ALit lit) = MLit lit
lowerTriv (AAloc aloc) = (MMloc . MAloc) aloc
lowerTriv (ALabel label) = MLabel label

regToExpr :: Reg -> M.Expr
regToExpr = M.Triv . MMloc . MRloc . Reg

alocToExpr :: Aloc -> M.Expr
alocToExpr = M.Triv . MMloc . MAloc
