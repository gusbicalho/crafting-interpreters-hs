{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
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
deriving instance (Show (Stmt f)) => Show (Program f)

data Stmt f = ExprStmt (f (Expr f))
            | VarDeclarationStmt (f (VarDeclaration f))
            | FunDeclarationStmt (f (FunDeclaration f))
            | ClassDeclarationStmt (f (ClassDeclaration f))
            | BlockStmt (f (Block f))
            | IfStmt (f (If f))
            | WhileStmt (f (While f))
            | ReturnStmt (f (Return f))
deriving instance ( Show (f (Expr f))
                  , Show (f (VarDeclaration f))
                  , Show (f (FunDeclaration f))
                  , Show (f (ClassDeclaration f))
                  , Show (f (Block f))
                  , Show (f (If f))
                  , Show (f (While f))
                  , Show (f (Return f))
                  ) => Show (Stmt f)

pattern ExprStmtI :: ExprI -> StmtI
pattern ExprStmtI expr = ExprStmt (Identity expr)

pattern VarDeclarationStmtI :: VarDeclaration Identity -> StmtI
pattern VarDeclarationStmtI expr = VarDeclarationStmt (Identity expr)

pattern FunDeclarationStmtI :: FunDeclaration Identity -> StmtI
pattern FunDeclarationStmtI expr = FunDeclarationStmt (Identity expr)

pattern ClassDeclarationStmtI :: ClassDeclaration Identity -> StmtI
pattern ClassDeclarationStmtI expr = ClassDeclarationStmt (Identity expr)

pattern BlockStmtI :: Block Identity -> StmtI
pattern BlockStmtI expr = BlockStmt (Identity expr)

pattern IfStmtI :: If Identity -> StmtI
pattern IfStmtI expr = IfStmt (Identity expr)

pattern WhileStmtI :: While Identity -> StmtI
pattern WhileStmtI expr = WhileStmt (Identity expr)

pattern ReturnStmtI :: Return Identity -> StmtI
pattern ReturnStmtI expr = ReturnStmt (Identity expr)

data VarDeclaration f = VarDeclaration { varDeclarationIdentifier :: Token
                                       , varDeclarationInitializer :: Expr f
                                       }
deriving instance (Show (Expr f)) => Show (VarDeclaration f)

data FunDeclaration f = FunDeclaration { funDeclarationIdentifier :: Token
                                       , funDeclarationInitializer :: Function f
                                       }
deriving instance (Show (Function f)) => Show (FunDeclaration f)

data ClassDeclaration f = ClassDeclaration { classDeclarationIdentifier :: Token
                                           , classDeclarationSuperclass :: Maybe (f Variable)
                                           , classDeclarationMethods :: Seq (f (Function f))
                                           }
deriving instance ( Show (f (Function f))
                  , Show (f Variable)
                  ) => Show (ClassDeclaration f)

newtype Block f = Block { blockBody :: Seq (Stmt f) }
deriving instance (Show (Stmt f)) => Show (Block f)

data If f = If { ifCondition :: Expr f
               , ifThenStmt :: Stmt f
               , ifElseStmt :: Maybe (Stmt f)
               }
deriving instance (Show (Stmt f), Show (Expr f)) => Show (If f)

data While f = While { whileCondition :: Expr f
                     , whileBody :: Stmt f
                     }
deriving instance (Show (Stmt f), Show (Expr f)) => Show (While f)

data Return f = Return { returnToken :: Token
                       , returnValue :: Maybe (Expr f)
                       }
deriving instance (Show (Expr f)) => Show (Return f)

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
pattern CallE :: ExprI -> Token -> Seq ExprI -> ExprI
pattern CallE callee paren args = CallExpr (Identity (Call callee paren args))

pattern GetPropertyExprI :: GetProperty Identity -> Expr Identity
pattern GetPropertyExprI e = GetPropertyExpr (Identity e)
pattern GetPropertyE :: ExprI -> Token -> ExprI
pattern GetPropertyE object name = GetPropertyExpr (Identity (GetProperty object name))

pattern SetPropertyExprI :: SetProperty Identity -> Expr Identity
pattern SetPropertyExprI e = SetPropertyExpr (Identity e)
pattern SetPropertyE :: ExprI -> Token -> ExprI -> ExprI
pattern SetPropertyE object name value = SetPropertyExpr (Identity (SetProperty object name value))

pattern ThisE :: Token -> ExprI
pattern ThisE tk = ThisExpr (Identity (This tk))

pattern SuperE :: Token -> Token -> ExprI
pattern SuperE keyword property = SuperExpr (Identity (Super keyword property))

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
            | GetPropertyExpr (f (GetProperty f))
            | SetPropertyExpr (f (SetProperty f))
            | ThisExpr (f This)
            | SuperExpr (f Super)
            | FunctionExpr (f (Function f))

deriving instance ( Show (f (Unary f))
                  , Show (f (Logical f))
                  , Show (f (Binary f))
                  , Show (f (Ternary f))
                  , Show (f (Grouping f))
                  , Show (f Literal)
                  , Show (f Variable)
                  , Show (f (Assignment f))
                  , Show (f (Call f))
                  , Show (f (GetProperty f))
                  , Show (f (SetProperty f))
                  , Show (f This)
                  , Show (f Super)
                  , Show (f (Function f))
                  ) => Show (Expr f)

data Ternary f = Ternary { ternaryLeft :: Expr f
                         , ternaryFirstOperator :: Token
                         , ternaryMiddle :: Expr f
                         , ternarySecondOperator :: Token
                         , ternaryRight :: Expr f
                         }
deriving instance (Show (Expr f)) => Show (Ternary f)

data Binary f = Binary { binaryLeft :: Expr f
                       , binaryOperator :: Token
                       , binaryRight :: Expr f
                       }
deriving instance (Show (Expr f)) => Show (Binary f)

data Logical f = Logical { logicalLeft :: Expr f
                         , logicalOperator :: Token
                         , logicalRight :: Expr f
                         }
deriving instance (Show (Expr f)) => Show (Logical f)

data Unary f = Unary { unaryOperator :: Token
                     , unaryRight :: Expr f
                     }
deriving instance (Show (Expr f)) => Show (Unary f)

newtype Grouping f = Grouping { groupingExpr :: Expr f }
deriving instance (Show (Expr f)) => Show (Grouping f)

data Function f = Function { functionToken :: Token
                           , functionArgs :: Seq Token
                           , functionBody :: Block f
                           }
deriving instance (Show (Block f)) => Show (Function f)

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
deriving instance (Show (Expr f)) => Show (Assignment f)

data Call f = Call { callCallee :: Expr f
                   , callParen :: Token
                   , callArguments :: Seq (Expr f)
                   }
deriving instance (Show (Expr f)) => Show (Call f)

data GetProperty f = GetProperty { getPropertyObject :: Expr f
                                 , getPropertyProperty :: Token
                                 }
deriving instance (Show (Expr f)) => Show (GetProperty f)

data SetProperty f = SetProperty { setPropertyObject :: Expr f
                                 , setPropertyProperty :: Token
                                 , setPropertyValue :: Expr f
                                 }
deriving instance (Show (Expr f)) => Show (SetProperty f)

newtype This = This { thisToken :: Token }
  deriving (Eq, Show)

data Super = Super { superKeyword :: Token
                   , superProperty :: Token
                   }
  deriving (Eq, Show)
