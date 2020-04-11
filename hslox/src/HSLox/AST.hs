module HSLox.AST where

import Data.Sequence (Seq)
import qualified Data.Text as T
import HSLox.Token (Token (..))

newtype Program = Program (Seq Stmt)
  deriving (Eq, Show, Ord)

data Stmt = ExprStmt Expr
          | PrintStmt Print
          | DeclarationStmt Declaration
          | BlockStmt Block
          | IfStmt If
          | WhileStmt While
  deriving (Eq, Show, Ord)

data Print = Print Token Expr
  deriving (Eq, Show, Ord)

data Declaration = VarDeclaration Token Expr
  deriving (Eq, Show, Ord)

newtype Block = Block (Seq Stmt)
  deriving (Eq, Show, Ord)

data If = If { ifCondition :: Expr
             , ifThenStmt :: Stmt
             , ifElseStmt :: Maybe Stmt
             }
  deriving (Eq, Show, Ord)

data While = While { whileCondition :: Expr
                   , whileBody :: Stmt
                   }
  deriving (Eq, Show, Ord)

pattern UnaryE :: Token -> Expr -> Expr
pattern UnaryE op expr = UnaryExpr (Unary op expr)

pattern LogicalE :: Expr -> Token -> Expr -> Expr
pattern LogicalE left op right = LogicalExpr (Logical left op right)

pattern BinaryE :: Expr -> Token -> Expr -> Expr
pattern BinaryE left op right = BinaryExpr (Binary left op right)

pattern TernaryE :: Expr -> Token -> Expr -> Token -> Expr -> Expr
pattern TernaryE left op1 middle op2 right = TernaryExpr (Ternary left op1 middle op2 right)

pattern GroupingE :: Expr -> Expr
pattern GroupingE expr = GroupingExpr (Grouping expr)

pattern VariableE :: Token -> Expr
pattern VariableE tk = VariableExpr (Variable tk)

pattern AssignmentE :: Token -> Expr -> Expr
pattern AssignmentE tk expr = AssignmentExpr (Assignment tk expr)

pattern StringE :: T.Text -> Expr
pattern StringE t = LiteralExpr (LitString t)

pattern NumE :: Double -> Expr
pattern NumE v = LiteralExpr (LitNum v)

pattern BoolE :: Bool -> Expr
pattern BoolE v = LiteralExpr (LitBool v)

pattern NilE :: Expr
pattern NilE = LiteralExpr LitNil

data Expr = UnaryExpr Unary
          | LogicalExpr Logical
          | BinaryExpr Binary
          | TernaryExpr Ternary
          | GroupingExpr Grouping
          | LiteralExpr Literal
          | VariableExpr Variable
          | AssignmentExpr Assignment
  deriving (Eq, Show, Ord)

data Ternary = Ternary { ternaryLeft :: Expr
                       , ternaryFirstOperator :: Token
                       , ternaryMiddle :: Expr
                       , ternarySecondOperator :: Token
                       , ternaryRight :: Expr
                       }
  deriving (Eq, Show, Ord)

data Binary = Binary { binaryLeft :: Expr
                     , binaryOperator :: Token
                     , binaryRight :: Expr
                     }
  deriving (Eq, Show, Ord)

data Logical = Logical { logicalLeft :: Expr
                       , logicalOperator :: Token
                       , logicalRight :: Expr
                       }
  deriving (Eq, Show, Ord)

data Unary = Unary { unaryOperator :: Token
                   , unaryRight :: Expr
                   }
  deriving (Eq, Show, Ord)

data Grouping = Grouping { groupingExpr :: Expr }
  deriving (Eq, Show, Ord)

data Literal
  = LitString T.Text
  | LitNum Double
  | LitBool Bool
  | LitNil
  deriving (Eq, Show, Ord)

newtype Variable = Variable Token
  deriving (Eq, Show, Ord)

data Assignment = Assignment Token Expr
  deriving (Eq, Show, Ord)
