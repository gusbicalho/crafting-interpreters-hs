{-# LANGUAGE UndecidableInstances #-}

module HSLox.ASTPrinter where

import Data.Foldable (Foldable (toList))
import Data.Text qualified as T
import HSLox.AST qualified as AST
import HSLox.AST.Meta (AsIdentity (..), Identity (runIdentity))
import HSLox.Token (Token (..))
import HSLox.Util qualified as Util

class ASTPrinter e where
  printAST :: e -> T.Text

instance (ASTPrinter e, AsIdentity f) => ASTPrinter (f e) where
  printAST =
    printAST
      . runIdentity
      . asIdentity

instance ASTPrinter (AST.Stmt f) => ASTPrinter (AST.Program f) where
  printAST (AST.Program stmts) = "[" <> foldMap ((" " <>) . printAST) stmts <> " ]"

instance
  ( ASTPrinter (f (AST.Expr f))
  , ASTPrinter (f (AST.VarDeclaration f))
  , ASTPrinter (f (AST.FunDeclaration f))
  , ASTPrinter (f (AST.ClassDeclaration f))
  , ASTPrinter (f (AST.Block f))
  , ASTPrinter (f (AST.If f))
  , ASTPrinter (f (AST.While f))
  , ASTPrinter (f (AST.Return f))
  ) =>
  ASTPrinter (AST.Stmt f)
  where
  printAST (AST.ExprStmt e) = printAST e
  printAST (AST.VarDeclarationStmt decl) = printAST decl
  printAST (AST.FunDeclarationStmt decl) = printAST decl
  printAST (AST.ClassDeclarationStmt decl) = printAST decl
  printAST (AST.BlockStmt block) = printAST block
  printAST (AST.IfStmt ifStmt) = printAST ifStmt
  printAST (AST.WhileStmt whileStmt) = printAST whileStmt
  printAST (AST.ReturnStmt returnStmt) = printAST returnStmt

instance
  ( ASTPrinter (AST.Expr f)
  ) =>
  ASTPrinter (AST.VarDeclaration f)
  where
  printAST (AST.VarDeclaration tk init) = parenthesize ("var " <> tokenLexeme tk) [init]

instance
  ( ASTPrinter (AST.Block f)
  ) =>
  ASTPrinter (AST.FunDeclaration f)
  where
  printAST (AST.FunDeclaration tkName (AST.Function _ _ args body)) =
    "(fun "
      <> tokenLexeme tkName
      <> " ["
      <> Util.foldMapIntersperse tokenLexeme " " args
      <> "] "
      <> printAST body
      <> ")"

instance
  ( ASTPrinter (f (AST.Function f))
  , ASTPrinter (f AST.Variable)
  ) =>
  ASTPrinter (AST.ClassDeclaration f)
  where
  printAST (AST.ClassDeclaration tkName superclass methods) =
    "(class "
      <> tokenLexeme tkName
      <> foldMap ((" < " <>) . printAST) superclass
      <> foldMap ((" " <>) . printAST) methods
      <> ")"

instance ASTPrinter (AST.Stmt f) => ASTPrinter (AST.Block f) where
  printAST (AST.Block stmts) = "{" <> foldMap ((" " <>) . printAST) stmts <> " }"

instance
  ( ASTPrinter (AST.Stmt f)
  , ASTPrinter (AST.Expr f)
  ) =>
  ASTPrinter (AST.If f)
  where
  printAST (AST.If cond thenStmt elseStmt) =
    "(if"
      <> " "
      <> printAST cond
      <> " "
      <> printAST thenStmt
      <> maybe "" ((" " <>) . printAST) elseStmt
      <> ")"

instance
  ( ASTPrinter (AST.Stmt f)
  , ASTPrinter (AST.Expr f)
  ) =>
  ASTPrinter (AST.While f)
  where
  printAST (AST.While cond body) =
    "(while"
      <> " "
      <> printAST cond
      <> " "
      <> printAST body
      <> ")"

instance ASTPrinter (AST.Block f) => ASTPrinter (AST.Function f) where
  printAST (AST.Function tk _ args body) =
    "("
      <> tokenLexeme tk
      <> " ["
      <> Util.foldMapIntersperse tokenLexeme " " args
      <> "] "
      <> printAST body
      <> ")"

instance ASTPrinter (AST.Expr f) => ASTPrinter (AST.Return f) where
  printAST (AST.Return tk expr) = parenthesize (tokenLexeme tk) (toList expr)

instance
  ( ASTPrinter (f (AST.Unary f))
  , ASTPrinter (f (AST.Logical f))
  , ASTPrinter (f (AST.Binary f))
  , ASTPrinter (f (AST.Ternary f))
  , ASTPrinter (f (AST.Grouping f))
  , ASTPrinter (f AST.Literal)
  , ASTPrinter (f AST.Variable)
  , ASTPrinter (f (AST.Assignment f))
  , ASTPrinter (f (AST.Call f))
  , ASTPrinter (f (AST.GetProperty f))
  , ASTPrinter (f (AST.SetProperty f))
  , ASTPrinter (f AST.This)
  , ASTPrinter (f AST.Super)
  , ASTPrinter (f (AST.Function f))
  ) =>
  ASTPrinter (AST.Expr f)
  where
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

instance ASTPrinter (AST.Expr f) => ASTPrinter (AST.Call f) where
  printAST (AST.Call callee _ args) = parenthesize (printAST callee) (toList args)

instance ASTPrinter (AST.Expr f) => ASTPrinter (AST.GetProperty f) where
  printAST (AST.GetProperty object tk) = parenthesize ("." <> tokenLexeme tk) [object]

instance ASTPrinter (AST.Expr f) => ASTPrinter (AST.SetProperty f) where
  printAST (AST.SetProperty object tk value) = parenthesize ("." <> tokenLexeme tk <> "=") [object, value]

instance ASTPrinter AST.This where
  printAST (AST.This tk) = tokenLexeme tk

instance ASTPrinter AST.Super where
  printAST (AST.Super keyword property) = tokenLexeme keyword <> "." <> tokenLexeme property

instance ASTPrinter (AST.Expr f) => ASTPrinter (AST.Assignment f) where
  printAST (AST.Assignment tk expr) = parenthesize ("= " <> tokenLexeme tk) [expr]

instance ASTPrinter AST.Variable where
  printAST (AST.Variable tk) = tokenLexeme tk

instance ASTPrinter AST.Literal where
  printAST (AST.LitString t) = T.pack . show $ t
  printAST (AST.LitBool t) = T.pack . show $ t
  printAST (AST.LitNum t) = T.pack . show $ t
  printAST AST.LitNil = "nil"

instance ASTPrinter (AST.Expr f) => ASTPrinter (AST.Grouping f) where
  printAST (AST.Grouping expr) = parenthesize "group" [expr]

instance ASTPrinter (AST.Expr f) => ASTPrinter (AST.Unary f) where
  printAST (AST.Unary op expr) = parenthesize (tokenLexeme op) [expr]

instance ASTPrinter (AST.Expr f) => ASTPrinter (AST.Binary f) where
  printAST (AST.Binary left op right) = parenthesize (tokenLexeme op) [left, right]

instance ASTPrinter (AST.Expr f) => ASTPrinter (AST.Logical f) where
  printAST (AST.Logical left op right) = parenthesize (tokenLexeme op) [left, right]

instance ASTPrinter (AST.Expr f) => ASTPrinter (AST.Ternary f) where
  printAST (AST.Ternary left op1 middle op2 right) =
    parenthesize
      (tokenLexeme op1 <> tokenLexeme op2)
      [left, middle, right]

parenthesize :: ASTPrinter (AST.Expr f) => T.Text -> [AST.Expr f] -> T.Text
parenthesize name exprs =
  "("
    <> name
    <> foldMap ((" " <>) . printAST) exprs
    <> ")"
