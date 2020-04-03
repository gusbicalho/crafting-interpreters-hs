module HSLox.AST where

import qualified Data.Text as T
import HSLox.Token (Token (..))

pattern UnaryE :: Token -> Expr -> Expr
pattern UnaryE op expr = UnaryExpr (Unary op expr)

pattern BinaryE :: Expr -> Token -> Expr -> Expr
pattern BinaryE left op right = BinaryExpr (Binary left op right)

pattern TernaryE :: Expr -> Token -> Expr -> Token -> Expr -> Expr
pattern TernaryE left op1 middle op2 right = TernaryExpr (Ternary left op1 middle op2 right)

pattern GroupingE :: Expr -> Expr
pattern GroupingE expr = GroupingExpr (Grouping expr)

pattern StringE :: T.Text -> Expr
pattern StringE t = ValueExpr (ValString t)

pattern NumE :: Double -> Expr
pattern NumE v = ValueExpr (ValNum v)

pattern BoolE :: Bool -> Expr
pattern BoolE v = ValueExpr (ValBool v)

pattern NilE :: Expr
pattern NilE = ValueExpr ValNil

data Expr = UnaryExpr Unary
          | BinaryExpr Binary
          | TernaryExpr Ternary
          | GroupingExpr Grouping
          | ValueExpr Value
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

data Unary = Unary { unaryOperator :: Token
                   , unaryRight :: Expr
                   }
  deriving (Eq, Show, Ord)

data Grouping = Grouping { groupingExpr :: Expr }
  deriving (Eq, Show, Ord)

data Value
  = ValString T.Text
  | ValNum Double
  | ValBool Bool
  | ValNil
  deriving (Eq, Show, Ord)
