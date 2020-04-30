module HSLox.AST.WalkAST
  ( WalkAST (..)
  ) where

import HSLox.AST
import HSLox.AST.AsAST

class WalkAST astNode where
  walkAST :: (Monad m, Traversable f, Traversable g, Functor h)
          => (forall astNode. AsAST (astNode f) f => f (astNode f) -> m (g (astNode f)))
          -> (forall astNode. AsAST (astNode h) h => g (astNode h) -> m (h (astNode h)))
          -> astNode f
          -> m (astNode h)

{-# INLINE walkLeaf #-}
walkLeaf :: ( Monad m, Traversable f, Traversable g, Functor h
            , AsAST (LeafNode a f) f, AsAST (LeafNode a h) h)
         => (forall astNode. AsAST (astNode f) f => f (astNode f) -> m (g (astNode f)))
         -> (forall astNode. AsAST (astNode h) h => g (astNode h) -> m (h (astNode h)))
         -> f a -> m (h a)
walkLeaf preWalk postWalk t = do
  preWalked <- preWalk (fmap LeafNode t)
  -- For a LeafNode, walking is a noop, just change the phantom functor type
  let walked = fmap (LeafNode . unLeafNode) preWalked
  postWalked <- postWalk walked
  pure $ fmap unLeafNode postWalked

instance WalkAST Program where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Program stmts) = Program <$> traverse (walkAST preWalk postWalk) stmts

instance WalkAST Stmt where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (ExprStmt t) = ExprStmt <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (VarDeclarationStmt t) = VarDeclarationStmt <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (FunDeclarationStmt t) = FunDeclarationStmt <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (ClassDeclarationStmt t) = ClassDeclarationStmt <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (BlockStmt t) = BlockStmt <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (IfStmt t) = IfStmt <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (WhileStmt t) = WhileStmt <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (ReturnStmt t) = ReturnStmt <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)

instance WalkAST VarDeclaration where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (VarDeclaration identifier expr) = VarDeclaration identifier <$> walkAST preWalk postWalk expr

instance WalkAST FunDeclaration where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (FunDeclaration identifier expr) = FunDeclaration identifier <$> walkAST preWalk postWalk expr

instance WalkAST ClassDeclaration where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (ClassDeclaration identifier methods) = ClassDeclaration identifier <$> traverse (walkAST preWalk postWalk) methods

instance WalkAST Block where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Block stmts) = Block <$> traverse (walkAST preWalk postWalk) stmts

instance WalkAST If where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (If cond thenStmt elseStmt) = If <$> walkAST preWalk postWalk cond
                                                        <*> walkAST preWalk postWalk thenStmt
                                                        <*> traverse (walkAST preWalk postWalk) elseStmt

instance WalkAST While where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (While cond body) = While <$> walkAST preWalk postWalk cond
                                                 <*> walkAST preWalk postWalk body

instance WalkAST Return where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Return tk expr) = Return tk <$> walkAST preWalk postWalk expr

instance WalkAST Expr where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (UnaryExpr t)       = UnaryExpr       <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (LogicalExpr t)     = LogicalExpr     <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (BinaryExpr t)      = BinaryExpr      <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (TernaryExpr t)     = TernaryExpr     <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (GroupingExpr t)    = GroupingExpr    <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (LiteralExpr t)     = LiteralExpr     <$> walkLeaf preWalk postWalk t
  walkAST preWalk postWalk (VariableExpr t)    = VariableExpr    <$> walkLeaf preWalk postWalk t
  walkAST preWalk postWalk (AssignmentExpr t)  = AssignmentExpr  <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (CallExpr t)        = CallExpr        <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (GetExpr t)         = GetExpr         <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (SetPropertyExpr t) = SetPropertyExpr <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)
  walkAST preWalk postWalk (FunctionExpr t)    = FunctionExpr    <$> (preWalk t >>= traverse (walkAST preWalk postWalk) >>= postWalk)

instance WalkAST Unary where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Unary op e) = Unary op <$> walkAST preWalk postWalk e

instance WalkAST Logical where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Logical left op right) = Logical <$> walkAST preWalk postWalk left
                                                         <*> pure op
                                                         <*> walkAST preWalk postWalk right

instance WalkAST Binary where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Binary left op right) = Binary <$> walkAST preWalk postWalk left
                                                       <*> pure op
                                                       <*> walkAST preWalk postWalk right

instance WalkAST Ternary where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Ternary left op1 middle op2 right) =
    Ternary <$> walkAST preWalk postWalk left
            <*> pure op1
            <*> walkAST preWalk postWalk middle
            <*> pure op2
            <*> walkAST preWalk postWalk right

instance WalkAST Grouping where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Grouping t) = Grouping <$> walkAST preWalk postWalk t

instance WalkAST Assignment where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Assignment op e) = Assignment op <$> walkAST preWalk postWalk e

instance WalkAST Call where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Call callee paren args) = Call <$> walkAST preWalk postWalk callee
                                                           <*> pure paren
                                                           <*> traverse (walkAST preWalk postWalk) args

instance WalkAST Get where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Get callee prop) = Get <$> walkAST preWalk postWalk callee
                                                   <*> pure prop

instance WalkAST SetProperty where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (SetProperty obj prop val) = SetProperty <$> walkAST preWalk postWalk obj
                                                                    <*> pure prop
                                                                    <*> walkAST preWalk postWalk val

instance WalkAST Function where
  {-# INLINE walkAST #-}
  walkAST preWalk postWalk (Function tk argNames body) = Function tk argNames <$> walkAST preWalk postWalk body
