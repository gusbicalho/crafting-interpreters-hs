module HSLox.AST.WalkAST (
  WalkAST (..),
  Walker (..),
  LeafWalker,
  NeutralWalker,
  (/>/),
  done,
  Walk,
) where

import Control.Monad ((>=>))
import HSLox.AST qualified as AST
import HSLox.AST.AsAST (AsAST, LeafNode (..))
import HSLox.AST.Meta (WithMeta)

type Walk input output m =
  forall astNode inner.
  AsAST astNode =>
  WithMeta input (astNode inner) ->
  m (WithMeta output (astNode inner))

data Walker input interIn interOut output m = Walker
  { preWalker :: !(Walk input interIn m)
  , postWalker :: !(Walk interOut output m)
  }

type LeafWalker input inter output = Walker input inter inter output
type NeutralWalker input output = Walker input input output output

done :: Applicative m => LeafWalker meta meta meta m
done = Walker pure pure

(/>/) ::
  forall input interIn1 interIn2 interOut2 interOut1 output m.
  Monad m =>
  Walker input interIn1 interOut1 output m ->
  Walker interIn1 interIn2 interOut2 interOut1 m ->
  Walker input interIn2 interOut2 output m
(/>/) = composeWalkers
 where
  composeWalkers (Walker preW1 postW1) (Walker preW2 postW2) =
    Walker (preW1 >=> preW2) (postW2 >=> postW1)
infixl 1 />/

class WalkAST astNode where
  walkAST ::
    Monad m =>
    LeafWalker input inter output m ->
    astNode input ->
    m (astNode output)

{-# INLINE walkLeaf #-}
walkLeaf ::
  ( Monad m
  , AsAST (LeafNode a)
  ) =>
  LeafWalker input inter output m ->
  WithMeta input a ->
  m (WithMeta output a)
walkLeaf (Walker preW postW) t = do
  preWed <- preW (fmap LeafNode t)
  -- For a LeafNode, walking is a noop, just change the phantom functor type
  let walked = fmap (LeafNode . unLeafNode) preWed
  postWalked <- postW walked
  pure $ fmap unLeafNode postWalked

{-# INLINE walkWrapped #-}
walkWrapped ::
  ( Monad m
  , WalkAST astNode
  , AsAST astNode
  ) =>
  LeafWalker input inter output m ->
  WithMeta input (astNode input) ->
  m (WithMeta output (astNode output))
walkWrapped walker@(Walker preW postW) = preW >=> traverse (walkAST walker) >=> postW

instance WalkAST AST.Program where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Program stmts) = AST.Program <$> traverse (walkAST walker) stmts

instance WalkAST AST.Stmt where
  {-# INLINE walkAST #-}
  walkAST walker (AST.ExprStmt t) = AST.ExprStmt <$> walkWrapped walker t
  walkAST walker (AST.VarDeclarationStmt t) = AST.VarDeclarationStmt <$> walkWrapped walker t
  walkAST walker (AST.FunDeclarationStmt t) = AST.FunDeclarationStmt <$> walkWrapped walker t
  walkAST walker (AST.ClassDeclarationStmt t) = AST.ClassDeclarationStmt <$> walkWrapped walker t
  walkAST walker (AST.BlockStmt t) = AST.BlockStmt <$> walkWrapped walker t
  walkAST walker (AST.IfStmt t) = AST.IfStmt <$> walkWrapped walker t
  walkAST walker (AST.WhileStmt t) = AST.WhileStmt <$> walkWrapped walker t
  walkAST walker (AST.ReturnStmt t) = AST.ReturnStmt <$> walkWrapped walker t

instance WalkAST AST.VarDeclaration where
  {-# INLINE walkAST #-}
  walkAST walker (AST.VarDeclaration identifier expr) =
    AST.VarDeclaration identifier <$> walkAST walker expr

instance WalkAST AST.FunDeclaration where
  {-# INLINE walkAST #-}
  walkAST walker (AST.FunDeclaration identifier expr) =
    AST.FunDeclaration identifier <$> walkAST walker expr

instance WalkAST AST.ClassDeclaration where
  {-# INLINE walkAST #-}
  walkAST walker (AST.ClassDeclaration identifier superclass methods) =
    AST.ClassDeclaration identifier <$> traverse (walkLeaf walker) superclass
      <*> traverse (walkWrapped walker) methods

instance WalkAST AST.Block where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Block stmts) = AST.Block <$> traverse (walkAST walker) stmts

instance WalkAST AST.If where
  {-# INLINE walkAST #-}
  walkAST walker (AST.If cond thenStmt elseStmt) =
    AST.If <$> walkAST walker cond
      <*> walkAST walker thenStmt
      <*> traverse (walkAST walker) elseStmt

instance WalkAST AST.While where
  {-# INLINE walkAST #-}
  walkAST walker (AST.While cond body) =
    AST.While <$> walkAST walker cond
      <*> walkAST walker body

instance WalkAST AST.Return where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Return tk expr) = AST.Return tk <$> traverse (walkAST walker) expr

instance WalkAST AST.Expr where
  {-# INLINE walkAST #-}
  walkAST walker (AST.UnaryExpr t) = AST.UnaryExpr <$> walkWrapped walker t
  walkAST walker (AST.LogicalExpr t) = AST.LogicalExpr <$> walkWrapped walker t
  walkAST walker (AST.BinaryExpr t) = AST.BinaryExpr <$> walkWrapped walker t
  walkAST walker (AST.TernaryExpr t) = AST.TernaryExpr <$> walkWrapped walker t
  walkAST walker (AST.GroupingExpr t) = AST.GroupingExpr <$> walkWrapped walker t
  walkAST walker (AST.LiteralExpr t) = AST.LiteralExpr <$> walkLeaf walker t
  walkAST walker (AST.VariableExpr t) = AST.VariableExpr <$> walkLeaf walker t
  walkAST walker (AST.AssignmentExpr t) = AST.AssignmentExpr <$> walkWrapped walker t
  walkAST walker (AST.CallExpr t) = AST.CallExpr <$> walkWrapped walker t
  walkAST walker (AST.GetPropertyExpr t) = AST.GetPropertyExpr <$> walkWrapped walker t
  walkAST walker (AST.ThisExpr t) = AST.ThisExpr <$> walkLeaf walker t
  walkAST walker (AST.SuperExpr t) = AST.SuperExpr <$> walkLeaf walker t
  walkAST walker (AST.SetPropertyExpr t) = AST.SetPropertyExpr <$> walkWrapped walker t
  walkAST walker (AST.FunctionExpr t) = AST.FunctionExpr <$> walkWrapped walker t

instance WalkAST AST.Unary where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Unary op e) = AST.Unary op <$> walkAST walker e

instance WalkAST AST.Logical where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Logical left op right) =
    AST.Logical <$> walkAST walker left
      <*> pure op
      <*> walkAST walker right

instance WalkAST AST.Binary where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Binary left op right) =
    AST.Binary <$> walkAST walker left
      <*> pure op
      <*> walkAST walker right

instance WalkAST AST.Ternary where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Ternary left op1 middle op2 right) =
    AST.Ternary <$> walkAST walker left
      <*> pure op1
      <*> walkAST walker middle
      <*> pure op2
      <*> walkAST walker right

instance WalkAST AST.Grouping where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Grouping t) = AST.Grouping <$> walkAST walker t

instance WalkAST AST.Assignment where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Assignment op e) = AST.Assignment op <$> walkAST walker e

instance WalkAST AST.Call where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Call callee paren args) =
    AST.Call <$> walkAST walker callee
      <*> pure paren
      <*> traverse (walkAST walker) args

instance WalkAST AST.GetProperty where
  {-# INLINE walkAST #-}
  walkAST walker (AST.GetProperty callee prop) =
    AST.GetProperty <$> walkAST walker callee
      <*> pure prop

instance WalkAST AST.SetProperty where
  {-# INLINE walkAST #-}
  walkAST walker (AST.SetProperty obj prop val) =
    AST.SetProperty <$> walkAST walker obj
      <*> pure prop
      <*> walkAST walker val

instance WalkAST AST.Function where
  {-# INLINE walkAST #-}
  walkAST walker (AST.Function tk tkRec argNames body) = AST.Function tk tkRec argNames <$> walkAST walker body
