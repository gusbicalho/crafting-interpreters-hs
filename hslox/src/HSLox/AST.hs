{-# LANGUAGE StrictData #-}
module HSLox.AST
  ( module HSLox.AST
  , Identity (..)
  ) where

import Data.Sequence (Seq)
import qualified Data.Text as T
import HSLox.AST.Meta
import HSLox.Token (Token (..))

type ProgramI = Program Identity
type StmtI = Stmt Identity
type ExprI = Expr Identity

newtype Program f = Program (Seq (Stmt f))

data Stmt f = ExprStmt (f (Expr f))
            | VarDeclarationStmt (f (VarDeclaration f))
            | BlockStmt (f (Block f))
            | IfStmt (f (If f))
            | WhileStmt (f (While f))
            | ReturnStmt (f (Return f))

pattern ExprStmtI :: ExprI -> StmtI
pattern ExprStmtI expr = ExprStmt (Identity expr)

pattern VarDeclarationStmtI :: VarDeclaration Identity -> StmtI
pattern VarDeclarationStmtI expr = VarDeclarationStmt (Identity expr)

pattern BlockStmtI :: Block Identity -> StmtI
pattern BlockStmtI expr = BlockStmt (Identity expr)

pattern IfStmtI :: If Identity -> StmtI
pattern IfStmtI expr = IfStmt (Identity expr)

pattern WhileStmtI :: While Identity -> StmtI
pattern WhileStmtI expr = WhileStmt (Identity expr)

pattern ReturnStmtI :: Return Identity -> StmtI
pattern ReturnStmtI expr = ReturnStmt (Identity expr)

instance FFunctor Stmt where
  ffmap nt (ExprStmt e) = ExprStmt $ nt (fmap (ffmap nt) e)
  ffmap nt (VarDeclarationStmt e) = VarDeclarationStmt $ nt (fmap (ffmap nt) e)
  ffmap nt (BlockStmt e) = BlockStmt $ nt (fmap (ffmap nt) e)
  ffmap nt (IfStmt e) = IfStmt $ nt (fmap (ffmap nt) e)
  ffmap nt (WhileStmt e) = WhileStmt $ nt (fmap (ffmap nt) e)
  ffmap nt (ReturnStmt e) = ReturnStmt $ nt (fmap (ffmap nt) e)

data VarDeclaration f = VarDeclaration { varDeclarationIdentifier :: Token
                                       , varDeclarationInitializer :: Expr f
                                       }
instance FFunctor VarDeclaration where
  ffmap nt (VarDeclaration tk e) = VarDeclaration tk (ffmap nt e)

newtype Block f = Block { blockBody :: (Seq (Stmt f)) }

instance FFunctor Block where
  ffmap nt (Block stmts) = Block (ffmap nt <$> stmts)

data If f = If { ifCondition :: Expr f
               , ifThenStmt :: Stmt f
               , ifElseStmt :: Maybe (Stmt f)
               }

instance FFunctor If where
  ffmap nt (If cond thenStmt elseStmt) = If (ffmap nt cond)
                                            (ffmap nt thenStmt)
                                            (ffmap nt <$> elseStmt)

data While f = While { whileCondition :: Expr f
                     , whileBody :: Stmt f
                     }

instance FFunctor While where
  ffmap nt (While cond body) = While (ffmap nt cond) (ffmap nt body)

data Return f = Return { returnToken :: Token
                       , returnValue :: Expr f
                       }

instance FFunctor Return where
  ffmap nt (Return tk expr) = Return tk (ffmap nt expr)

pattern UnaryExprI :: Unary Identity -> Expr Identity
pattern UnaryExprI e = UnaryExpr (Identity e)
pattern UnaryE :: Token -> ExprI -> ExprI
pattern UnaryE op expr = UnaryExpr (Identity (Unary op expr))

pattern LogicalExprI :: Logical Identity -> Expr Identity
pattern LogicalExprI e = LogicalExpr (Identity e)
pattern LogicalE :: ExprI -> Token -> ExprI -> ExprI
pattern LogicalE left op right = LogicalExpr (Identity (Logical left op right))

pattern BinaryExprI :: Binary Identity -> Expr Identity
pattern BinaryExprI e = BinaryExpr (Identity e)
pattern BinaryE :: ExprI -> Token -> ExprI -> ExprI
pattern BinaryE left op right = BinaryExpr (Identity (Binary left op right))

pattern TernaryExprI :: Ternary Identity -> Expr Identity
pattern TernaryExprI e = TernaryExpr (Identity e)
pattern TernaryE :: ExprI -> Token -> ExprI -> Token -> ExprI -> ExprI
pattern TernaryE left op1 middle op2 right = TernaryExpr (Identity (Ternary left op1 middle op2 right))

pattern GroupingExprI :: Grouping Identity -> Expr Identity
pattern GroupingExprI e = GroupingExpr (Identity e)
pattern GroupingE :: ExprI -> ExprI
pattern GroupingE expr = GroupingExpr (Identity (Grouping expr))

pattern VariableExprI :: Variable -> Expr Identity
pattern VariableExprI e = VariableExpr (Identity e)
pattern VariableE :: Token -> ExprI
pattern VariableE tk = VariableExpr (Identity (Variable tk))

pattern AssignmentExprI :: Assignment Identity -> Expr Identity
pattern AssignmentExprI e = AssignmentExpr (Identity e)
pattern AssignmentE :: Token -> ExprI -> ExprI
pattern AssignmentE tk expr = AssignmentExpr (Identity (Assignment tk expr))

pattern CallExprI :: Call Identity -> Expr Identity
pattern CallExprI e = CallExpr (Identity e)
pattern CallE :: ExprI -> Token -> Seq (ExprI) -> ExprI
pattern CallE callee paren args = CallExpr (Identity (Call callee paren args))

pattern LiteralExprI :: Literal -> Expr Identity
pattern LiteralExprI e = LiteralExpr (Identity e)

pattern StringE :: T.Text -> ExprI
pattern StringE t = LiteralExpr (Identity (LitString t))

pattern NumE :: Double -> ExprI
pattern NumE v = LiteralExpr (Identity (LitNum v))

pattern BoolE :: Bool -> ExprI
pattern BoolE v = LiteralExpr (Identity (LitBool v))

pattern NilE :: ExprI
pattern NilE = LiteralExpr (Identity LitNil)

pattern FunctionExprI :: Function Identity -> Expr Identity
pattern FunctionExprI e = FunctionExpr (Identity e)
pattern FunctionE :: Token -> Seq Token -> Block Identity -> ExprI
pattern FunctionE tk args body = FunctionExpr (Identity (Function tk args body))

data Expr f = UnaryExpr (f (Unary f))
            | LogicalExpr (f (Logical f))
            | BinaryExpr (f (Binary f))
            | TernaryExpr (f (Ternary f))
            | GroupingExpr (f (Grouping f))
            | LiteralExpr (f Literal)
            | VariableExpr (f Variable)
            | AssignmentExpr (f (Assignment f))
            | CallExpr (f (Call f))
            | FunctionExpr (f (Function f))

instance FFunctor Expr where
  ffmap nt (UnaryExpr e) = UnaryExpr $ nt (fmap (ffmap nt) e)
  ffmap nt (LogicalExpr e) = LogicalExpr $ nt (fmap (ffmap nt) e)
  ffmap nt (BinaryExpr e) = BinaryExpr $ nt (fmap (ffmap nt) e)
  ffmap nt (TernaryExpr e) = TernaryExpr $ nt (fmap (ffmap nt) e)
  ffmap nt (GroupingExpr e) = GroupingExpr $ nt (fmap (ffmap nt) e)
  ffmap nt (LiteralExpr e) = LiteralExpr $ nt e
  ffmap nt (VariableExpr e) = VariableExpr $ nt e
  ffmap nt (AssignmentExpr e) = AssignmentExpr $ nt (fmap (ffmap nt) e)
  ffmap nt (CallExpr e) = CallExpr $ nt (fmap (ffmap nt) e)
  ffmap nt (FunctionExpr e) = FunctionExpr $ nt (fmap (ffmap nt) e)

data Ternary f = Ternary { ternaryLeft :: Expr f
                         , ternaryFirstOperator :: Token
                         , ternaryMiddle :: Expr f
                         , ternarySecondOperator :: Token
                         , ternaryRight :: Expr f
                         }
instance FFunctor Ternary where
  ffmap nt (Ternary left op1 middle op2 right) = Ternary (ffmap nt left)
                                                         op1
                                                         (ffmap nt middle)
                                                         op2
                                                         (ffmap nt right)

data Binary f = Binary { binaryLeft :: Expr f
                       , binaryOperator :: Token
                       , binaryRight :: Expr f
                       }
instance FFunctor Binary where
  ffmap nt (Binary left op right) = Binary (ffmap nt left)
                                           op
                                           (ffmap nt right)

data Logical f = Logical { logicalLeft :: Expr f
                         , logicalOperator :: Token
                         , logicalRight :: Expr f
                         }
instance FFunctor Logical where
  ffmap nt (Logical left op right) = Logical (ffmap nt left)
                                             op
                                             (ffmap nt right)

data Unary f = Unary { unaryOperator :: Token
                     , unaryRight :: Expr f
                     }
instance FFunctor Unary where
  ffmap nt (Unary op expr) = Unary op (ffmap nt expr)

newtype Grouping f = Grouping { groupingExpr :: Expr f }
instance FFunctor Grouping where
  ffmap nt (Grouping expr) = Grouping (ffmap nt expr)

data Function f = Function { functionToken :: Token
                           , functionArgs :: Seq Token
                           , functionBody :: Block f
                           }
instance FFunctor Function where
  ffmap nt (Function tk args body) = Function tk args (ffmap nt body)

data Literal
  = LitString T.Text
  | LitNum Double
  | LitBool Bool
  | LitNil
  deriving (Eq, Show, Ord)

newtype Variable = Variable { variableIdentifier :: Token }
  deriving (Eq, Show, Ord)

data Assignment f = Assignment { assignmentIdentifier :: Token
                               , assignmentRValue :: Expr f
                               }
instance FFunctor Assignment where
  ffmap nt (Assignment tk expr) = Assignment tk (ffmap nt expr)

data Call f = Call { callCallee :: Expr f
                   , callParen :: Token
                   , callArguments :: Seq (Expr f)
                   }
instance FFunctor Call where
  ffmap nt (Call callee tk exprs) = Call (ffmap nt callee) tk (ffmap nt <$> exprs)
