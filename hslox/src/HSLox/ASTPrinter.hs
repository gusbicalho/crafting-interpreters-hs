module HSLox.ASTPrinter where

import qualified Data.Text as T
import HSLox.AST
import HSLox.Token (Token (..))

class ASTPrinter e where
  printAST :: e -> T.Text

instance ASTPrinter Program where
  printAST (Program stmts) = "[" <> foldMap ((" " <>) . printAST) stmts <> " ]"

instance ASTPrinter Stmt where
  printAST (ExprStmt e) = printAST e
  printAST (PrintStmt print) = printAST print
  printAST (DeclarationStmt decl) = printAST decl
  printAST (BlockStmt block) = printAST block
  printAST (IfStmt ifStmt) = printAST ifStmt

instance ASTPrinter Print where
  printAST (Print tk expr) = parenthesize (tokenLexeme tk) [expr]

instance ASTPrinter Declaration where
  printAST (VarDeclaration tk init) = parenthesize ("var " <> tokenLexeme tk) [init]

instance ASTPrinter Block where
  printAST (Block stmts) = "{" <> foldMap ((" " <>) . printAST) stmts <> " }"

instance ASTPrinter If where
  printAST (If cond thenStmt elseStmt) = "(if"
                                      <> " " <> printAST cond
                                      <> " " <> printAST thenStmt
                                      <> maybe "" ((" " <>) . printAST) elseStmt
                                      <> ")"

instance ASTPrinter Expr where
  printAST (UnaryExpr e) = printAST e
  printAST (BinaryExpr e) = printAST e
  printAST (TernaryExpr e) = printAST e
  printAST (GroupingExpr e) = printAST e
  printAST (LiteralExpr e) = printAST e
  printAST (VariableExpr e) = printAST e
  printAST (AssignmentExpr e) = printAST e

instance ASTPrinter Assignment where
  printAST (Assignment tk expr) = parenthesize ("= " <> tokenLexeme tk) [expr]

instance ASTPrinter Variable where
  printAST (Variable tk) = tokenLexeme tk

instance ASTPrinter Literal where
  printAST (LitString t) = T.pack . show $ t
  printAST (LitBool t) = T.pack . show $ t
  printAST (LitNum t) = T.pack . show $ t
  printAST LitNil = "nil"

instance ASTPrinter Grouping where
  printAST (Grouping expr) = parenthesize "group" [expr]

instance ASTPrinter Unary where
  printAST (Unary op expr) = parenthesize (tokenLexeme op) [expr]

instance ASTPrinter Binary where
  printAST (Binary left op right) = parenthesize (tokenLexeme op) [left, right]

instance ASTPrinter Ternary where
  printAST (Ternary left op1 middle op2 right)
    = parenthesize ((tokenLexeme op1) <> (tokenLexeme op2))
                   [left, middle, right]

parenthesize :: T.Text -> [Expr] -> T.Text
parenthesize name exprs = "("
                       <> name
                       <> foldMap ((" " <>) . printAST) exprs
                       <> ")"
