module HSLox.ASTPrinter where

import qualified Data.Text as T
import HSLox.AST
import HSLox.Token (Token (..))

class ASTPrinter e where
  printAST :: e -> T.Text

instance ASTPrinter Expr where
  printAST (UnaryExpr e) = printAST e
  printAST (BinaryExpr e) = printAST e
  printAST (GroupingExpr e) = printAST e
  printAST (ValueExpr e) = printAST e

instance ASTPrinter Value where
  printAST (ValString t) = T.pack . show $ t
  printAST (ValBool t) = T.pack . show $ t
  printAST (ValNum t) = T.pack . show $ t
  printAST ValNil = "nil"

instance ASTPrinter Grouping where
  printAST (Grouping expr) = parenthesize "group" [expr]

instance ASTPrinter Unary where
  printAST (Unary op expr) = parenthesize (tokenLexeme op) [expr]

instance ASTPrinter Binary where
  printAST (Binary left op right) = parenthesize (tokenLexeme op) [left, right]

parenthesize :: T.Text -> [Expr] -> T.Text
parenthesize name exprs = "("
                       <> name
                       <> (foldMap ((" " <>) . printAST) exprs)
                       <> ")"
