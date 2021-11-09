module HSLox.AST.AsAST where

import HSLox.AST (
  Assignment,
  Binary,
  Block,
  Call,
  ClassDeclaration,
  Expr,
  FunDeclaration,
  Function,
  GetProperty,
  Grouping,
  If,
  Literal,
  Logical,
  Return,
  SetProperty,
  Stmt,
  Super,
  Ternary,
  This,
  Unary,
  VarDeclaration,
  Variable,
  While,
 )

class AsAST a meta | a -> meta where
  {-# INLINE toExpr #-}
  toExpr :: a -> Maybe (Expr meta)
  toExpr _ = Nothing
  {-# INLINE toStmt #-}
  toStmt :: a -> Maybe (Stmt meta)
  toStmt _ = Nothing
  {-# INLINE toVarDeclaration #-}
  toVarDeclaration :: a -> Maybe (VarDeclaration meta)
  toVarDeclaration _ = Nothing
  {-# INLINE toFunDeclaration #-}
  toFunDeclaration :: a -> Maybe (FunDeclaration meta)
  toFunDeclaration _ = Nothing
  {-# INLINE toClassDeclaration #-}
  toClassDeclaration :: a -> Maybe (ClassDeclaration meta)
  toClassDeclaration _ = Nothing
  {-# INLINE toBlock #-}
  toBlock :: a -> Maybe (Block meta)
  toBlock _ = Nothing
  {-# INLINE toIf #-}
  toIf :: a -> Maybe (If meta)
  toIf _ = Nothing
  {-# INLINE toWhile #-}
  toWhile :: a -> Maybe (While meta)
  toWhile _ = Nothing
  {-# INLINE toReturn #-}
  toReturn :: a -> Maybe (Return meta)
  toReturn _ = Nothing
  {-# INLINE toUnary #-}
  toUnary :: a -> Maybe (Unary meta)
  toUnary _ = Nothing
  {-# INLINE toLogical #-}
  toLogical :: a -> Maybe (Logical meta)
  toLogical _ = Nothing
  {-# INLINE toBinary #-}
  toBinary :: a -> Maybe (Binary meta)
  toBinary _ = Nothing
  {-# INLINE toTernary #-}
  toTernary :: a -> Maybe (Ternary meta)
  toTernary _ = Nothing
  {-# INLINE toGrouping #-}
  toGrouping :: a -> Maybe (Grouping meta)
  toGrouping _ = Nothing
  {-# INLINE toAssignment #-}
  toAssignment :: a -> Maybe (Assignment meta)
  toAssignment _ = Nothing
  {-# INLINE toCall #-}
  toCall :: a -> Maybe (Call meta)
  toCall _ = Nothing
  {-# INLINE toGetProperty #-}
  toGetProperty :: a -> Maybe (GetProperty meta)
  toGetProperty _ = Nothing
  {-# INLINE toSetProperty #-}
  toSetProperty :: a -> Maybe (SetProperty meta)
  toSetProperty _ = Nothing
  {-# INLINE toFunction #-}
  toFunction :: a -> Maybe (Function meta)
  toFunction _ = Nothing
  {-# INLINE toVariable #-}
  toVariable :: a -> Maybe Variable
  toVariable _ = Nothing
  {-# INLINE toLiteral #-}
  toLiteral :: a -> Maybe Literal
  toLiteral _ = Nothing
  {-# INLINE toThis #-}
  toThis :: a -> Maybe This
  toThis _ = Nothing
  {-# INLINE toSuper #-}
  toSuper :: a -> Maybe Super
  toSuper _ = Nothing

instance AsAST (Stmt meta) meta where
  {-# INLINE toStmt #-}
  toStmt = Just
instance AsAST (VarDeclaration meta) meta where
  {-# INLINE toVarDeclaration #-}
  toVarDeclaration = Just
instance AsAST (FunDeclaration meta) meta where
  {-# INLINE toFunDeclaration #-}
  toFunDeclaration = Just
instance AsAST (ClassDeclaration meta) meta where
  {-# INLINE toClassDeclaration #-}
  toClassDeclaration = Just
instance AsAST (Block meta) meta where
  {-# INLINE toBlock #-}
  toBlock = Just
instance AsAST (If meta) meta where
  {-# INLINE toIf #-}
  toIf = Just
instance AsAST (While meta) meta where
  {-# INLINE toWhile #-}
  toWhile = Just
instance AsAST (Return meta) meta where
  {-# INLINE toReturn #-}
  toReturn = Just
instance AsAST (Expr meta) meta where
  {-# INLINE toExpr #-}
  toExpr = Just
instance AsAST (Unary meta) meta where
  {-# INLINE toUnary #-}
  toUnary = Just
instance AsAST (Logical meta) meta where
  {-# INLINE toLogical #-}
  toLogical = Just
instance AsAST (Binary meta) meta where
  {-# INLINE toBinary #-}
  toBinary = Just
instance AsAST (Ternary meta) meta where
  {-# INLINE toTernary #-}
  toTernary = Just
instance AsAST (Grouping meta) meta where
  {-# INLINE toGrouping #-}
  toGrouping = Just
instance AsAST (Assignment meta) meta where
  {-# INLINE toAssignment #-}
  toAssignment = Just
instance AsAST (Call meta) meta where
  {-# INLINE toCall #-}
  toCall = Just
instance AsAST (GetProperty meta) meta where
  {-# INLINE toGetProperty #-}
  toGetProperty = Just
instance AsAST (SetProperty meta) meta where
  {-# INLINE toSetProperty #-}
  toSetProperty = Just
instance AsAST (Function meta) meta where
  {-# INLINE toFunction #-}
  toFunction = Just

newtype LeafNode a meta = LeafNode {unLeafNode :: a}

instance AsAST (LeafNode Variable meta) meta where
  {-# INLINE toVariable #-}
  toVariable = Just . unLeafNode
instance AsAST (LeafNode Literal meta) meta where
  {-# INLINE toLiteral #-}
  toLiteral = Just . unLeafNode
instance AsAST (LeafNode This meta) meta where
  {-# INLINE toThis #-}
  toThis = Just . unLeafNode
instance AsAST (LeafNode Super meta) meta where
  {-# INLINE toSuper #-}
  toSuper = Just . unLeafNode
