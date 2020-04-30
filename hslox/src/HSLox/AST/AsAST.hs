module HSLox.AST.AsAST where

import Data.Kind
import HSLox.AST

class AsAST a f | a -> f where
  {-# INLINE toExpr #-}
  toExpr :: a -> Maybe (Expr f)
  toExpr _ = Nothing
  {-# INLINE toStmt #-}
  toStmt :: a -> Maybe (Stmt f)
  toStmt _ = Nothing
  {-# INLINE toVarDeclaration #-}
  toVarDeclaration :: a -> Maybe (VarDeclaration f)
  toVarDeclaration _ = Nothing
  {-# INLINE toFunDeclaration #-}
  toFunDeclaration :: a -> Maybe (FunDeclaration f)
  toFunDeclaration _ = Nothing
  {-# INLINE toClassDeclaration #-}
  toClassDeclaration :: a -> Maybe (ClassDeclaration f)
  toClassDeclaration _ = Nothing
  {-# INLINE toBlock #-}
  toBlock :: a -> Maybe (Block f)
  toBlock _ = Nothing
  {-# INLINE toIf #-}
  toIf :: a -> Maybe (If f)
  toIf _ = Nothing
  {-# INLINE toWhile #-}
  toWhile :: a -> Maybe (While f)
  toWhile _ = Nothing
  {-# INLINE toReturn #-}
  toReturn :: a -> Maybe (Return f)
  toReturn _ = Nothing
  {-# INLINE toUnary #-}
  toUnary :: a -> Maybe (Unary f)
  toUnary _ = Nothing
  {-# INLINE toLogical #-}
  toLogical :: a -> Maybe (Logical f)
  toLogical _ = Nothing
  {-# INLINE toBinary #-}
  toBinary :: a -> Maybe (Binary f)
  toBinary _ = Nothing
  {-# INLINE toTernary #-}
  toTernary :: a -> Maybe (Ternary f)
  toTernary _ = Nothing
  {-# INLINE toGrouping #-}
  toGrouping :: a -> Maybe (Grouping f)
  toGrouping _ = Nothing
  {-# INLINE toAssignment #-}
  toAssignment :: a -> Maybe (Assignment f)
  toAssignment _ = Nothing
  {-# INLINE toCall #-}
  toCall :: a -> Maybe (Call f)
  toCall _ = Nothing
  {-# INLINE toGet #-}
  toGet :: a -> Maybe (Get f)
  toGet _ = Nothing
  {-# INLINE toSetProperty #-}
  toSetProperty :: a -> Maybe (SetProperty f)
  toSetProperty _ = Nothing
  {-# INLINE toFunction #-}
  toFunction :: a -> Maybe (Function f)
  toFunction _ = Nothing
  {-# INLINE toVariable #-}
  toVariable :: a -> Maybe Variable
  toVariable _ = Nothing
  {-# INLINE toLiteral #-}
  toLiteral :: a -> Maybe Literal
  toLiteral _ = Nothing

instance AsAST (Stmt f) f where
  {-# INLINE toStmt #-}
  toStmt = Just
instance AsAST (VarDeclaration f) f where
  {-# INLINE toVarDeclaration #-}
  toVarDeclaration = Just
instance AsAST (FunDeclaration f) f where
  {-# INLINE toFunDeclaration #-}
  toFunDeclaration = Just
instance AsAST (ClassDeclaration f) f where
  {-# INLINE toClassDeclaration #-}
  toClassDeclaration = Just
instance AsAST (Block f) f where
  {-# INLINE toBlock #-}
  toBlock = Just
instance AsAST (If f) f where
  {-# INLINE toIf #-}
  toIf = Just
instance AsAST (While f) f where
  {-# INLINE toWhile #-}
  toWhile = Just
instance AsAST (Return f) f where
  {-# INLINE toReturn #-}
  toReturn = Just
instance AsAST (Expr f) f where
  {-# INLINE toExpr #-}
  toExpr = Just
instance AsAST (Unary f) f where
  {-# INLINE toUnary #-}
  toUnary = Just
instance AsAST (Logical f) f where
  {-# INLINE toLogical #-}
  toLogical = Just
instance AsAST (Binary f) f where
  {-# INLINE toBinary #-}
  toBinary = Just
instance AsAST (Ternary f) f where
  {-# INLINE toTernary #-}
  toTernary = Just
instance AsAST (Grouping f) f where
  {-# INLINE toGrouping #-}
  toGrouping = Just
instance AsAST (Assignment f) f where
  {-# INLINE toAssignment #-}
  toAssignment = Just
instance AsAST (Call f) f where
  {-# INLINE toCall #-}
  toCall = Just
instance AsAST (Get f) f where
  {-# INLINE toGet #-}
  toGet = Just
instance AsAST (SetProperty f) f where
  {-# INLINE toSetProperty #-}
  toSetProperty = Just
instance AsAST (Function f) f where
  {-# INLINE toFunction #-}
  toFunction = Just

newtype LeafNode a (f :: Type -> Type) = LeafNode { unLeafNode :: a }

instance AsAST (LeafNode Variable f) f where
  {-# INLINE toVariable #-}
  toVariable = Just . unLeafNode
instance AsAST (LeafNode Literal f) f where
  {-# INLINE toLiteral #-}
  toLiteral = Just . unLeafNode
