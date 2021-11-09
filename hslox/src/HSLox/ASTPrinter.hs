module HSLox.ASTPrinter where

import Data.Foldable (Foldable (toList))
import Data.Text qualified as T
import HSLox.AST qualified as AST
import HSLox.AST.Meta (WithMeta)
import HSLox.AST.Meta qualified as WithMeta
import HSLox.Token (Token (..))
import HSLox.Util qualified as Util

class ASTPrinter e where
  printAST :: e -> T.Text

instance (ASTPrinter e) => ASTPrinter (WithMeta meta e) where
  printAST = printAST . WithMeta.content

instance ASTPrinter (AST.Program meta) where
  printAST (AST.Program stmts) = "[" <> foldMap ((" " <>) . printAST) stmts <> " ]"

instance ASTPrinter (AST.Stmt meta) where
  printAST (AST.ExprStmt e) = printAST e
  printAST (AST.VarDeclarationStmt decl) = printAST decl
  printAST (AST.FunDeclarationStmt decl) = printAST decl
  printAST (AST.ClassDeclarationStmt decl) = printAST decl
  printAST (AST.BlockStmt block) = printAST block
  printAST (AST.IfStmt ifStmt) = printAST ifStmt
  printAST (AST.WhileStmt whileStmt) = printAST whileStmt
  printAST (AST.ReturnStmt returnStmt) = printAST returnStmt

instance ASTPrinter (AST.VarDeclaration meta) where
  printAST (AST.VarDeclaration tk init) = parenthesize ("var " <> tokenLexeme tk) [init]

instance ASTPrinter (AST.FunDeclaration meta) where
  printAST (AST.FunDeclaration tkName (AST.Function _ _ args body)) =
    "(fun "
      <> tokenLexeme tkName
      <> " ["
      <> Util.foldMapIntersperse tokenLexeme " " args
      <> "] "
      <> printAST body
      <> ")"

instance ASTPrinter (AST.ClassDeclaration meta) where
  printAST (AST.ClassDeclaration tkName superclass methods) =
    "(class "
      <> tokenLexeme tkName
      <> foldMap ((" < " <>) . printAST) superclass
      <> foldMap ((" " <>) . printAST) methods
      <> ")"

instance ASTPrinter (AST.Block meta) where
  printAST (AST.Block stmts) = "{" <> foldMap ((" " <>) . printAST) stmts <> " }"

instance ASTPrinter (AST.If meta) where
  printAST (AST.If cond thenStmt elseStmt) =
    "(if"
      <> " "
      <> printAST cond
      <> " "
      <> printAST thenStmt
      <> maybe "" ((" " <>) . printAST) elseStmt
      <> ")"

instance ASTPrinter (AST.While meta) where
  printAST (AST.While cond body) =
    "(while"
      <> " "
      <> printAST cond
      <> " "
      <> printAST body
      <> ")"

instance ASTPrinter (AST.Function meta) where
  printAST (AST.Function tk _ args body) =
    "("
      <> tokenLexeme tk
      <> " ["
      <> Util.foldMapIntersperse tokenLexeme " " args
      <> "] "
      <> printAST body
      <> ")"

instance ASTPrinter (AST.Return meta) where
  printAST (AST.Return tk expr) = parenthesize (tokenLexeme tk) (toList expr)

instance ASTPrinter (AST.Expr meta) where
  printAST (AST.UnaryExpr e) = printAST e
  printAST (AST.LogicalExpr e) = printAST e
  printAST (AST.BinaryExpr e) = printAST e
  printAST (AST.TernaryExpr e) = printAST e
  printAST (AST.GroupingExpr e) = printAST e
  printAST (AST.LiteralExpr e) = printAST e
  printAST (AST.VariableExpr e) = printAST e
  printAST (AST.AssignmentExpr e) = printAST e
  printAST (AST.CallExpr e) = printAST e
  printAST (AST.GetPropertyExpr e) = printAST e
  printAST (AST.SetPropertyExpr e) = printAST e
  printAST (AST.ThisExpr e) = printAST e
  printAST (AST.SuperExpr e) = printAST e
  printAST (AST.FunctionExpr e) = printAST e

instance ASTPrinter (AST.Call meta) where
  printAST (AST.Call callee _ args) = parenthesize (printAST callee) (toList args)

instance ASTPrinter (AST.GetProperty meta) where
  printAST (AST.GetProperty object tk) = parenthesize ("." <> tokenLexeme tk) [object]

instance ASTPrinter (AST.SetProperty meta) where
  printAST (AST.SetProperty object tk value) = parenthesize ("." <> tokenLexeme tk <> "=") [object, value]

instance ASTPrinter AST.This where
  printAST (AST.This tk) = tokenLexeme tk

instance ASTPrinter AST.Super where
  printAST (AST.Super keyword property) = tokenLexeme keyword <> "." <> tokenLexeme property

instance ASTPrinter (AST.Assignment meta) where
  printAST (AST.Assignment tk expr) = parenthesize ("= " <> tokenLexeme tk) [expr]

instance ASTPrinter AST.Variable where
  printAST (AST.Variable tk) = tokenLexeme tk

instance ASTPrinter AST.Literal where
  printAST (AST.LitString t) = T.pack . show $ t
  printAST (AST.LitBool t) = T.pack . show $ t
  printAST (AST.LitNum t) = T.pack . show $ t
  printAST AST.LitNil = "nil"

instance ASTPrinter (AST.Grouping meta) where
  printAST (AST.Grouping expr) = parenthesize "group" [expr]

instance ASTPrinter (AST.Unary meta) where
  printAST (AST.Unary op expr) = parenthesize (tokenLexeme op) [expr]

instance ASTPrinter (AST.Binary meta) where
  printAST (AST.Binary left op right) = parenthesize (tokenLexeme op) [left, right]

instance ASTPrinter (AST.Logical meta) where
  printAST (AST.Logical left op right) = parenthesize (tokenLexeme op) [left, right]

instance ASTPrinter (AST.Ternary meta) where
  printAST (AST.Ternary left op1 middle op2 right) =
    parenthesize
      (tokenLexeme op1 <> tokenLexeme op2)
      [left, middle, right]

parenthesize :: T.Text -> [AST.Expr meta] -> T.Text
parenthesize name exprs =
  "("
    <> name
    <> foldMap ((" " <>) . printAST) exprs
    <> ")"
