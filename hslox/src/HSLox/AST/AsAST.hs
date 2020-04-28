module HSLox.AST.AsAST where

import Data.Kind
import HSLox.AST

class AsAST a f | a -> f where
  toExpr :: a -> Maybe (Expr f)
  toExpr _ = Nothing
  toStmt :: a -> Maybe (Stmt f)
  toStmt _ = Nothing
  toVarDeclaration :: a -> Maybe (VarDeclaration f)
  toVarDeclaration _ = Nothing
  toBlock :: a -> Maybe (Block f)
  toBlock _ = Nothing
  toIf :: a -> Maybe (If f)
  toIf _ = Nothing
  toWhile :: a -> Maybe (While f)
  toWhile _ = Nothing
  toReturn :: a -> Maybe (Return f)
  toReturn _ = Nothing
  toUnary :: a -> Maybe (Unary f)
  toUnary _ = Nothing
  toLogical :: a -> Maybe (Logical f)
  toLogical _ = Nothing
  toBinary :: a -> Maybe (Binary f)
  toBinary _ = Nothing
  toTernary :: a -> Maybe (Ternary f)
  toTernary _ = Nothing
  toGrouping :: a -> Maybe (Grouping f)
  toGrouping _ = Nothing
  toAssignment :: a -> Maybe (Assignment f)
  toAssignment _ = Nothing
  toCall :: a -> Maybe (Call f)
  toCall _ = Nothing
  toFunction :: a -> Maybe (Function f)
  toFunction _ = Nothing
  toVariable :: a -> Maybe Variable
  toVariable _ = Nothing
  toLiteral :: a -> Maybe Literal
  toLiteral _ = Nothing

instance AsAST (Stmt f) f where
  toStmt = Just
instance AsAST (VarDeclaration f) f where
  toVarDeclaration = Just
instance AsAST (Block f) f where
  toBlock = Just
instance AsAST (If f) f where
  toIf = Just
instance AsAST (While f) f where
  toWhile = Just
instance AsAST (Return f) f where
  toReturn = Just
instance AsAST (Expr f) f where
  toExpr = Just
instance AsAST (Unary f) f where
  toUnary = Just
instance AsAST (Logical f) f where
  toLogical = Just
instance AsAST (Binary f) f where
  toBinary = Just
instance AsAST (Ternary f) f where
  toTernary = Just
instance AsAST (Grouping f) f where
  toGrouping = Just
instance AsAST (Assignment f) f where
  toAssignment = Just
instance AsAST (Call f) f where
  toCall = Just
instance AsAST (Function f) f where
  toFunction = Just

newtype LeafNode a (f :: Type -> Type) = LeafNode { unLeafNode :: a }

instance AsAST (LeafNode Variable f) f where
  toVariable = Just . unLeafNode
instance AsAST (LeafNode Literal f) f where
  toLiteral = Just . unLeafNode
