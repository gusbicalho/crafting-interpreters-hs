module HSLox.AST where

import qualified Data.Text as T
import HSLox.Token (Token (..))

unary :: Token -> Expr -> Expr
unary op expr = UnaryExpr $ Unary op expr

binary :: Expr -> Token -> Expr -> Expr
binary left op right = BinaryExpr $ Binary left op right

grouping :: Expr -> Expr
grouping = GroupingExpr . Grouping

stringValue :: T.Text -> Expr
stringValue = ValueExpr . ValString

numValue :: Double -> Expr
numValue = ValueExpr . ValNum

boolValue :: Bool -> Expr
boolValue = ValueExpr . ValBool

nilValue :: Expr
nilValue = ValueExpr ValNil


data Expr = UnaryExpr Unary
          | BinaryExpr Binary
          | GroupingExpr Grouping
          | ValueExpr Value
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
