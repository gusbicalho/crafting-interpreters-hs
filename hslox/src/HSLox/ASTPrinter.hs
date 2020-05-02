{-# LANGUAGE UndecidableInstances #-}
module HSLox.ASTPrinter where

import Data.Foldable
import qualified Data.Text as T
import HSLox.AST
import HSLox.AST.Meta
import HSLox.Token (Token (..))
import qualified HSLox.Util as Util

class ASTPrinter e where
  printAST :: e -> T.Text

instance (ASTPrinter e, AsIdentity f) => ASTPrinter (f e) where
  printAST = printAST
           . runIdentity
           . asIdentity

instance ASTPrinter (Stmt f) => ASTPrinter (Program f) where
  printAST (Program stmts) = "[" <> foldMap ((" " <>) . printAST) stmts <> " ]"

instance ( ASTPrinter (f (Expr f))
         , ASTPrinter (f (VarDeclaration f))
         , ASTPrinter (f (FunDeclaration f))
         , ASTPrinter (f (ClassDeclaration f))
         , ASTPrinter (f (Block f))
         , ASTPrinter (f (If f))
         , ASTPrinter (f (While f))
         , ASTPrinter (f (Return f))
         ) => ASTPrinter (Stmt f) where
  printAST (ExprStmt e) = printAST e
  printAST (VarDeclarationStmt decl) = printAST decl
  printAST (FunDeclarationStmt decl) = printAST decl
  printAST (ClassDeclarationStmt decl) = printAST decl
  printAST (BlockStmt block) = printAST block
  printAST (IfStmt ifStmt) = printAST ifStmt
  printAST (WhileStmt whileStmt) = printAST whileStmt
  printAST (ReturnStmt returnStmt) = printAST returnStmt

instance ( ASTPrinter (Expr f)
         ) => ASTPrinter (VarDeclaration f) where
  printAST (VarDeclaration tk init) = parenthesize ("var " <> tokenLexeme tk) [init]

instance ( ASTPrinter (Block f)
         ) => ASTPrinter (FunDeclaration f) where
  printAST (FunDeclaration tkName (Function _ args body))
      = "(fun "
      <> tokenLexeme tkName
      <> " [" <> Util.foldMapIntersperse tokenLexeme " " args <> "] "
      <> printAST body
      <> ")"

instance ( ASTPrinter (f (Function f))
         , ASTPrinter (f Variable)
         ) => ASTPrinter (ClassDeclaration f) where
  printAST (ClassDeclaration tkName superclass methods)
      = "(class "
      <> tokenLexeme tkName
      <> foldMap ((" < " <>) . printAST) superclass
      <> foldMap ((" " <>) . printAST) methods
      <> ")"

instance ASTPrinter (Stmt f) => ASTPrinter (Block f) where
  printAST (Block stmts) = "{" <> foldMap ((" " <>) . printAST) stmts <> " }"

instance ( ASTPrinter (Stmt f)
         , ASTPrinter (Expr f)
         ) => ASTPrinter (If f) where
  printAST (If cond thenStmt elseStmt) = "(if"
                                      <> " " <> printAST cond
                                      <> " " <> printAST thenStmt
                                      <> maybe "" ((" " <>) . printAST) elseStmt
                                      <> ")"

instance ( ASTPrinter (Stmt f)
         , ASTPrinter (Expr f)
         ) => ASTPrinter (While f) where
  printAST (While cond body) = "(while"
                            <> " " <> printAST cond
                            <> " " <> printAST body
                            <> ")"

instance ASTPrinter (Block f) => ASTPrinter (Function f) where
  printAST (Function tk args body)
    = "("
   <> tokenLexeme tk
   <> " [" <> Util.foldMapIntersperse tokenLexeme " " args <> "] "
   <> printAST body
   <> ")"

instance ASTPrinter (Expr f) => ASTPrinter (Return f) where
  printAST (Return tk expr) = parenthesize (tokenLexeme tk) (toList expr)

instance ( ASTPrinter (f (Unary f))
         , ASTPrinter (f (Logical f))
         , ASTPrinter (f (Binary f))
         , ASTPrinter (f (Ternary f))
         , ASTPrinter (f (Grouping f))
         , ASTPrinter (f Literal)
         , ASTPrinter (f Variable)
         , ASTPrinter (f (Assignment f))
         , ASTPrinter (f (Call f))
         , ASTPrinter (f (GetProperty f))
         , ASTPrinter (f (SetProperty f))
         , ASTPrinter (f This)
         , ASTPrinter (f Super)
         , ASTPrinter (f (Function f))
         ) => ASTPrinter (Expr f) where
  printAST (UnaryExpr e) = printAST e
  printAST (LogicalExpr e) = printAST e
  printAST (BinaryExpr e) = printAST e
  printAST (TernaryExpr e) = printAST e
  printAST (GroupingExpr e) = printAST e
  printAST (LiteralExpr e) = printAST e
  printAST (VariableExpr e) = printAST e
  printAST (AssignmentExpr e) = printAST e
  printAST (CallExpr e) = printAST e
  printAST (GetPropertyExpr e) = printAST e
  printAST (SetPropertyExpr e) = printAST e
  printAST (ThisExpr e) = printAST e
  printAST (SuperExpr e) = printAST e
  printAST (FunctionExpr e) = printAST e

instance ASTPrinter (Expr f) => ASTPrinter (Call f) where
  printAST (Call callee _ args) = parenthesize (printAST callee) (toList args)

instance ASTPrinter (Expr f) => ASTPrinter (GetProperty f) where
  printAST (GetProperty object tk) = parenthesize ("." <> tokenLexeme tk) [object]

instance ASTPrinter (Expr f) => ASTPrinter (SetProperty f) where
  printAST (SetProperty object tk value) = parenthesize ("." <> tokenLexeme tk <> "=") [object, value]

instance ASTPrinter This where
  printAST (This tk) = tokenLexeme tk

instance ASTPrinter Super where
  printAST (Super keyword property) = tokenLexeme keyword <> "." <> tokenLexeme property

instance ASTPrinter (Expr f) => ASTPrinter (Assignment f) where
  printAST (Assignment tk expr) = parenthesize ("= " <> tokenLexeme tk) [expr]

instance ASTPrinter Variable where
  printAST (Variable tk) = tokenLexeme tk

instance ASTPrinter Literal where
  printAST (LitString t) = T.pack . show $ t
  printAST (LitBool t) = T.pack . show $ t
  printAST (LitNum t) = T.pack . show $ t
  printAST LitNil = "nil"

instance ASTPrinter (Expr f) => ASTPrinter (Grouping f) where
  printAST (Grouping expr) = parenthesize "group" [expr]

instance ASTPrinter (Expr f) => ASTPrinter (Unary f) where
  printAST (Unary op expr) = parenthesize (tokenLexeme op) [expr]

instance ASTPrinter (Expr f) => ASTPrinter (Binary f) where
  printAST (Binary left op right) = parenthesize (tokenLexeme op) [left, right]

instance ASTPrinter (Expr f) => ASTPrinter (Logical f) where
  printAST (Logical left op right) = parenthesize (tokenLexeme op) [left, right]

instance ASTPrinter (Expr f) => ASTPrinter (Ternary f) where
  printAST (Ternary left op1 middle op2 right)
    = parenthesize (tokenLexeme op1 <> tokenLexeme op2)
                   [left, middle, right]

parenthesize :: ASTPrinter (Expr f) => T.Text -> [Expr f] -> T.Text
parenthesize name exprs = "("
                       <> name
                       <> foldMap ((" " <>) . printAST) exprs
                       <> ")"
