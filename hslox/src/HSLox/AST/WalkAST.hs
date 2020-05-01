module HSLox.AST.WalkAST
  ( WalkAST (..)
  ) where

import Control.Monad
import HSLox.AST
import HSLox.AST.AsAST

type PreWalk input m = forall astNode
                        . AsAST (astNode input) input
                       => input (astNode input) -> m (input (astNode input))

type PostWalk input output m = forall astNode
                                . AsAST (astNode output) output
                               => input (astNode output) -> m (output (astNode output))

class WalkAST astNode where
  walkAST :: (Monad m, Traversable input, Functor output)
          => PreWalk input m
          -> PostWalk input output m
          -> astNode input -> m (astNode output)

{-# INLINE walkLeaf #-}
walkLeaf :: ( Monad m, Traversable input, Functor output
            , AsAST (LeafNode a input) input, AsAST (LeafNode a output) output)
         => PreWalk input m
         -> PostWalk input output m
         -> input a -> m (output a)
walkLeaf preW postW t = do
  preWed <- preW (fmap LeafNode t)
  -- For a LeafNode, walking is a noop, just change the phantom functor type
  let walked = fmap (LeafNode . unLeafNode) preWed
  postWalked <- postW walked
  pure $ fmap unLeafNode postWalked

{-# INLINE walkWrapped #-}
walkWrapped :: ( Monad m
               , WalkAST astNode
               , AsAST (astNode input) input
               , AsAST (astNode output) output
               , Traversable input
               , Functor output )
            => PreWalk input m
            -> PostWalk input output m
            -> input (astNode input) -> m (output (astNode output))
walkWrapped preW postW = preW >=> traverse (walkAST preW postW) >=> postW

instance WalkAST Program where
  {-# INLINE walkAST #-}
  walkAST preW postW (Program stmts) = Program <$> traverse (walkAST preW postW) stmts

instance WalkAST Stmt where
  {-# INLINE walkAST #-}
  walkAST preW postW (ExprStmt t)             = ExprStmt             <$> walkWrapped preW postW t
  walkAST preW postW (VarDeclarationStmt t)   = VarDeclarationStmt   <$> walkWrapped preW postW t
  walkAST preW postW (FunDeclarationStmt t)   = FunDeclarationStmt   <$> walkWrapped preW postW t
  walkAST preW postW (ClassDeclarationStmt t) = ClassDeclarationStmt <$> walkWrapped preW postW t
  walkAST preW postW (BlockStmt t)            = BlockStmt            <$> walkWrapped preW postW t
  walkAST preW postW (IfStmt t)               = IfStmt               <$> walkWrapped preW postW t
  walkAST preW postW (WhileStmt t)            = WhileStmt            <$> walkWrapped preW postW t
  walkAST preW postW (ReturnStmt t)           = ReturnStmt           <$> walkWrapped preW postW t

instance WalkAST VarDeclaration where
  {-# INLINE walkAST #-}
  walkAST preW postW (VarDeclaration identifier expr) =
    VarDeclaration identifier <$> walkAST preW postW expr

instance WalkAST FunDeclaration where
  {-# INLINE walkAST #-}
  walkAST preW postW (FunDeclaration identifier expr) =
    FunDeclaration identifier <$> walkAST preW postW expr

instance WalkAST ClassDeclaration where
  {-# INLINE walkAST #-}
  walkAST preW postW (ClassDeclaration identifier superclass methods) =
    ClassDeclaration identifier <$> traverse (walkLeaf preW postW) superclass
                                <*> traverse (walkWrapped preW postW) methods

instance WalkAST Block where
  {-# INLINE walkAST #-}
  walkAST preW postW (Block stmts) = Block <$> traverse (walkAST preW postW) stmts

instance WalkAST If where
  {-# INLINE walkAST #-}
  walkAST preW postW (If cond thenStmt elseStmt) = If <$> walkAST preW postW cond
                                                      <*> walkAST preW postW thenStmt
                                                      <*> traverse (walkAST preW postW) elseStmt

instance WalkAST While where
  {-# INLINE walkAST #-}
  walkAST preW postW (While cond body) = While <$> walkAST preW postW cond
                                               <*> walkAST preW postW body

instance WalkAST Return where
  {-# INLINE walkAST #-}
  walkAST preW postW (Return tk expr) = Return tk <$> traverse (walkAST preW postW) expr

instance WalkAST Expr where
  {-# INLINE walkAST #-}
  walkAST preW postW (UnaryExpr t)       = UnaryExpr       <$> walkWrapped preW postW t
  walkAST preW postW (LogicalExpr t)     = LogicalExpr     <$> walkWrapped preW postW t
  walkAST preW postW (BinaryExpr t)      = BinaryExpr      <$> walkWrapped preW postW t
  walkAST preW postW (TernaryExpr t)     = TernaryExpr     <$> walkWrapped preW postW t
  walkAST preW postW (GroupingExpr t)    = GroupingExpr    <$> walkWrapped preW postW t
  walkAST preW postW (LiteralExpr t)     = LiteralExpr     <$> walkLeaf preW postW t
  walkAST preW postW (VariableExpr t)    = VariableExpr    <$> walkLeaf preW postW t
  walkAST preW postW (AssignmentExpr t)  = AssignmentExpr  <$> walkWrapped preW postW t
  walkAST preW postW (CallExpr t)        = CallExpr        <$> walkWrapped preW postW t
  walkAST preW postW (GetExpr t)         = GetExpr         <$> walkWrapped preW postW t
  walkAST preW postW (ThisExpr t)        = ThisExpr        <$> walkLeaf preW postW t
  walkAST preW postW (SetPropertyExpr t) = SetPropertyExpr <$> walkWrapped preW postW t
  walkAST preW postW (FunctionExpr t)    = FunctionExpr    <$> walkWrapped preW postW t

instance WalkAST Unary where
  {-# INLINE walkAST #-}
  walkAST preW postW (Unary op e) = Unary op <$> walkAST preW postW e

instance WalkAST Logical where
  {-# INLINE walkAST #-}
  walkAST preW postW (Logical left op right) = Logical <$> walkAST preW postW left
                                                       <*> pure op
                                                       <*> walkAST preW postW right

instance WalkAST Binary where
  {-# INLINE walkAST #-}
  walkAST preW postW (Binary left op right) = Binary <$> walkAST preW postW left
                                                     <*> pure op
                                                     <*> walkAST preW postW right

instance WalkAST Ternary where
  {-# INLINE walkAST #-}
  walkAST preW postW (Ternary left op1 middle op2 right) =
    Ternary <$> walkAST preW postW left
            <*> pure op1
            <*> walkAST preW postW middle
            <*> pure op2
            <*> walkAST preW postW right

instance WalkAST Grouping where
  {-# INLINE walkAST #-}
  walkAST preW postW (Grouping t) = Grouping <$> walkAST preW postW t

instance WalkAST Assignment where
  {-# INLINE walkAST #-}
  walkAST preW postW (Assignment op e) = Assignment op <$> walkAST preW postW e

instance WalkAST Call where
  {-# INLINE walkAST #-}
  walkAST preW postW (Call callee paren args) = Call <$> walkAST preW postW callee
                                                     <*> pure paren
                                                     <*> traverse (walkAST preW postW) args

instance WalkAST Get where
  {-# INLINE walkAST #-}
  walkAST preW postW (Get callee prop) = Get <$> walkAST preW postW callee
                                             <*> pure prop

instance WalkAST SetProperty where
  {-# INLINE walkAST #-}
  walkAST preW postW (SetProperty obj prop val) = SetProperty <$> walkAST preW postW obj
                                                              <*> pure prop
                                                              <*> walkAST preW postW val

instance WalkAST Function where
  {-# INLINE walkAST #-}
  walkAST preW postW (Function tk argNames body) = Function tk argNames <$> walkAST preW postW body
