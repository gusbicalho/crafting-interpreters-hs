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

class AsAST a where
  {-# INLINE toExpr #-}
  toExpr :: a meta -> Maybe (Expr meta)
  toExpr _ = Nothing
  {-# INLINE toStmt #-}
  toStmt :: a meta -> Maybe (Stmt meta)
  toStmt _ = Nothing
  {-# INLINE toVarDeclaration #-}
  toVarDeclaration :: a meta -> Maybe (VarDeclaration meta)
  toVarDeclaration _ = Nothing
  {-# INLINE toFunDeclaration #-}
  toFunDeclaration :: a meta -> Maybe (FunDeclaration meta)
  toFunDeclaration _ = Nothing
  {-# INLINE toClassDeclaration #-}
  toClassDeclaration :: a meta -> Maybe (ClassDeclaration meta)
  toClassDeclaration _ = Nothing
  {-# INLINE toBlock #-}
  toBlock :: a meta -> Maybe (Block meta)
  toBlock _ = Nothing
  {-# INLINE toIf #-}
  toIf :: a meta -> Maybe (If meta)
  toIf _ = Nothing
  {-# INLINE toWhile #-}
  toWhile :: a meta -> Maybe (While meta)
  toWhile _ = Nothing
  {-# INLINE toReturn #-}
  toReturn :: a meta -> Maybe (Return meta)
  toReturn _ = Nothing
  {-# INLINE toUnary #-}
  toUnary :: a meta -> Maybe (Unary meta)
  toUnary _ = Nothing
  {-# INLINE toLogical #-}
  toLogical :: a meta -> Maybe (Logical meta)
  toLogical _ = Nothing
  {-# INLINE toBinary #-}
  toBinary :: a meta -> Maybe (Binary meta)
  toBinary _ = Nothing
  {-# INLINE toTernary #-}
  toTernary :: a meta -> Maybe (Ternary meta)
  toTernary _ = Nothing
  {-# INLINE toGrouping #-}
  toGrouping :: a meta -> Maybe (Grouping meta)
  toGrouping _ = Nothing
  {-# INLINE toAssignment #-}
  toAssignment :: a meta -> Maybe (Assignment meta)
  toAssignment _ = Nothing
  {-# INLINE toCall #-}
  toCall :: a meta -> Maybe (Call meta)
  toCall _ = Nothing
  {-# INLINE toGetProperty #-}
  toGetProperty :: a meta -> Maybe (GetProperty meta)
  toGetProperty _ = Nothing
  {-# INLINE toSetProperty #-}
  toSetProperty :: a meta -> Maybe (SetProperty meta)
  toSetProperty _ = Nothing
  {-# INLINE toFunction #-}
  toFunction :: a meta -> Maybe (Function meta)
  toFunction _ = Nothing
  {-# INLINE toVariable #-}
  toVariable :: a meta -> Maybe Variable
  toVariable _ = Nothing
  {-# INLINE toLiteral #-}
  toLiteral :: a meta -> Maybe Literal
  toLiteral _ = Nothing
  {-# INLINE toThis #-}
  toThis :: a meta -> Maybe This
  toThis _ = Nothing
  {-# INLINE toSuper #-}
  toSuper :: a meta -> Maybe Super
  toSuper _ = Nothing

instance AsAST Stmt where
  {-# INLINE toStmt #-}
  toStmt = Just
instance AsAST VarDeclaration where
  {-# INLINE toVarDeclaration #-}
  toVarDeclaration = Just
instance AsAST FunDeclaration where
  {-# INLINE toFunDeclaration #-}
  toFunDeclaration = Just
instance AsAST ClassDeclaration where
  {-# INLINE toClassDeclaration #-}
  toClassDeclaration = Just
instance AsAST Block where
  {-# INLINE toBlock #-}
  toBlock = Just
instance AsAST If where
  {-# INLINE toIf #-}
  toIf = Just
instance AsAST While where
  {-# INLINE toWhile #-}
  toWhile = Just
instance AsAST Return where
  {-# INLINE toReturn #-}
  toReturn = Just
instance AsAST Expr where
  {-# INLINE toExpr #-}
  toExpr = Just
instance AsAST Unary where
  {-# INLINE toUnary #-}
  toUnary = Just
instance AsAST Logical where
  {-# INLINE toLogical #-}
  toLogical = Just
instance AsAST Binary where
  {-# INLINE toBinary #-}
  toBinary = Just
instance AsAST Ternary where
  {-# INLINE toTernary #-}
  toTernary = Just
instance AsAST Grouping where
  {-# INLINE toGrouping #-}
  toGrouping = Just
instance AsAST Assignment where
  {-# INLINE toAssignment #-}
  toAssignment = Just
instance AsAST Call where
  {-# INLINE toCall #-}
  toCall = Just
instance AsAST GetProperty where
  {-# INLINE toGetProperty #-}
  toGetProperty = Just
instance AsAST SetProperty where
  {-# INLINE toSetProperty #-}
  toSetProperty = Just
instance AsAST Function where
  {-# INLINE toFunction #-}
  toFunction = Just

newtype LeafNode a meta = LeafNode {unLeafNode :: a}

instance AsAST (LeafNode Variable) where
  {-# INLINE toVariable #-}
  toVariable = Just . unLeafNode
instance AsAST (LeafNode Literal) where
  {-# INLINE toLiteral #-}
  toLiteral = Just . unLeafNode
instance AsAST (LeafNode This) where
  {-# INLINE toThis #-}
  toThis = Just . unLeafNode
instance AsAST (LeafNode Super) where
  {-# INLINE toSuper #-}
  toSuper = Just . unLeafNode
