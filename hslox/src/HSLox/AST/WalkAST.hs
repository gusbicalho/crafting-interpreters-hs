module HSLox.AST.WalkAST (
  WalkAST (..),
) where

import Control.Monad ((>=>))
import HSLox.AST qualified as AST
import HSLox.AST.AsAST (AsAST, LeafNode (..))

type PreWalk input m =
  forall astNode.
  AsAST (astNode input) input =>
  input (astNode input) ->
  m (input (astNode input))

type PostWalk input output m =
  forall astNode.
  AsAST (astNode output) output =>
  input (astNode output) ->
  m (output (astNode output))

class WalkAST astNode where
  walkAST ::
    (Monad m, Traversable input, Functor output) =>
    PreWalk input m ->
    PostWalk input output m ->
    astNode input ->
    m (astNode output)

{-# INLINE walkLeaf #-}
walkLeaf ::
  ( Monad m
  , Traversable input
  , Functor output
  , AsAST (LeafNode a input) input
  , AsAST (LeafNode a output) output
  ) =>
  PreWalk input m ->
  PostWalk input output m ->
  input a ->
  m (output a)
walkLeaf preW postW t = do
  preWed <- preW (fmap LeafNode t)
  -- For a LeafNode, walking is a noop, just change the phantom functor type
  let walked = fmap (LeafNode . unLeafNode) preWed
  postWalked <- postW walked
  pure $ fmap unLeafNode postWalked

{-# INLINE walkWrapped #-}
walkWrapped ::
  ( Monad m
  , WalkAST astNode
  , AsAST (astNode input) input
  , AsAST (astNode output) output
  , Traversable input
  , Functor output
  ) =>
  PreWalk input m ->
  PostWalk input output m ->
  input (astNode input) ->
  m (output (astNode output))
walkWrapped preW postW = preW >=> traverse (walkAST preW postW) >=> postW

instance WalkAST AST.Program where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Program stmts) = AST.Program <$> traverse (walkAST preW postW) stmts

instance WalkAST AST.Stmt where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.ExprStmt t) = AST.ExprStmt <$> walkWrapped preW postW t
  walkAST preW postW (AST.VarDeclarationStmt t) = AST.VarDeclarationStmt <$> walkWrapped preW postW t
  walkAST preW postW (AST.FunDeclarationStmt t) = AST.FunDeclarationStmt <$> walkWrapped preW postW t
  walkAST preW postW (AST.ClassDeclarationStmt t) = AST.ClassDeclarationStmt <$> walkWrapped preW postW t
  walkAST preW postW (AST.BlockStmt t) = AST.BlockStmt <$> walkWrapped preW postW t
  walkAST preW postW (AST.IfStmt t) = AST.IfStmt <$> walkWrapped preW postW t
  walkAST preW postW (AST.WhileStmt t) = AST.WhileStmt <$> walkWrapped preW postW t
  walkAST preW postW (AST.ReturnStmt t) = AST.ReturnStmt <$> walkWrapped preW postW t

instance WalkAST AST.VarDeclaration where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.VarDeclaration identifier expr) =
    AST.VarDeclaration identifier <$> walkAST preW postW expr

instance WalkAST AST.FunDeclaration where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.FunDeclaration identifier expr) =
    AST.FunDeclaration identifier <$> walkAST preW postW expr

instance WalkAST AST.ClassDeclaration where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.ClassDeclaration identifier superclass methods) =
    AST.ClassDeclaration identifier <$> traverse (walkLeaf preW postW) superclass
      <*> traverse (walkWrapped preW postW) methods

instance WalkAST AST.Block where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Block stmts) = AST.Block <$> traverse (walkAST preW postW) stmts

instance WalkAST AST.If where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.If cond thenStmt elseStmt) =
    AST.If <$> walkAST preW postW cond
      <*> walkAST preW postW thenStmt
      <*> traverse (walkAST preW postW) elseStmt

instance WalkAST AST.While where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.While cond body) =
    AST.While <$> walkAST preW postW cond
      <*> walkAST preW postW body

instance WalkAST AST.Return where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Return tk expr) = AST.Return tk <$> traverse (walkAST preW postW) expr

instance WalkAST AST.Expr where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.UnaryExpr t) = AST.UnaryExpr <$> walkWrapped preW postW t
  walkAST preW postW (AST.LogicalExpr t) = AST.LogicalExpr <$> walkWrapped preW postW t
  walkAST preW postW (AST.BinaryExpr t) = AST.BinaryExpr <$> walkWrapped preW postW t
  walkAST preW postW (AST.TernaryExpr t) = AST.TernaryExpr <$> walkWrapped preW postW t
  walkAST preW postW (AST.GroupingExpr t) = AST.GroupingExpr <$> walkWrapped preW postW t
  walkAST preW postW (AST.LiteralExpr t) = AST.LiteralExpr <$> walkLeaf preW postW t
  walkAST preW postW (AST.VariableExpr t) = AST.VariableExpr <$> walkLeaf preW postW t
  walkAST preW postW (AST.AssignmentExpr t) = AST.AssignmentExpr <$> walkWrapped preW postW t
  walkAST preW postW (AST.CallExpr t) = AST.CallExpr <$> walkWrapped preW postW t
  walkAST preW postW (AST.GetPropertyExpr t) = AST.GetPropertyExpr <$> walkWrapped preW postW t
  walkAST preW postW (AST.ThisExpr t) = AST.ThisExpr <$> walkLeaf preW postW t
  walkAST preW postW (AST.SuperExpr t) = AST.SuperExpr <$> walkLeaf preW postW t
  walkAST preW postW (AST.SetPropertyExpr t) = AST.SetPropertyExpr <$> walkWrapped preW postW t
  walkAST preW postW (AST.FunctionExpr t) = AST.FunctionExpr <$> walkWrapped preW postW t

instance WalkAST AST.Unary where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Unary op e) = AST.Unary op <$> walkAST preW postW e

instance WalkAST AST.Logical where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Logical left op right) =
    AST.Logical <$> walkAST preW postW left
      <*> pure op
      <*> walkAST preW postW right

instance WalkAST AST.Binary where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Binary left op right) =
    AST.Binary <$> walkAST preW postW left
      <*> pure op
      <*> walkAST preW postW right

instance WalkAST AST.Ternary where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Ternary left op1 middle op2 right) =
    AST.Ternary <$> walkAST preW postW left
      <*> pure op1
      <*> walkAST preW postW middle
      <*> pure op2
      <*> walkAST preW postW right

instance WalkAST AST.Grouping where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Grouping t) = AST.Grouping <$> walkAST preW postW t

instance WalkAST AST.Assignment where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Assignment op e) = AST.Assignment op <$> walkAST preW postW e

instance WalkAST AST.Call where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Call callee paren args) =
    AST.Call <$> walkAST preW postW callee
      <*> pure paren
      <*> traverse (walkAST preW postW) args

instance WalkAST AST.GetProperty where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.GetProperty callee prop) =
    AST.GetProperty <$> walkAST preW postW callee
      <*> pure prop

instance WalkAST AST.SetProperty where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.SetProperty obj prop val) =
    AST.SetProperty <$> walkAST preW postW obj
      <*> pure prop
      <*> walkAST preW postW val

instance WalkAST AST.Function where
  {-# INLINE walkAST #-}
  walkAST preW postW (AST.Function tk tkRec argNames body) = AST.Function tk tkRec argNames <$> walkAST preW postW body
