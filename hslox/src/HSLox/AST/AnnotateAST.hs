module HSLox.AST.AnnotateAST
  ( AnnotateAST (..)
  ) where

import Data.Kind
import HSLox.AST
import HSLox.AST.AsAST

class AnnotateAST astNode where
  annotateAST :: (Monad m, Traversable f, Functor g)
              => (forall a h. AsAST a h => f a -> m (g a))
              -> astNode f
              -> m (astNode g)

newtype OpaqueNode a (f :: Type -> Type) = OpaqueNode { unOpaqueNode :: a }
instance AsAST (OpaqueNode a f) f where
annotateOpaque :: (Functor f, Functor g, Functor m)
               => (forall a h. f (OpaqueNode a h) -> m (g (OpaqueNode a h)))
               -> f a -> m (g a)
annotateOpaque annotate t = (fmap . fmap) unOpaqueNode (annotate (fmap OpaqueNode t))

instance AnnotateAST Stmt where
  annotateAST annotate (ExprStmt t) = ExprStmt <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (VarDeclarationStmt t) = VarDeclarationStmt <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (BlockStmt t) = BlockStmt <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (IfStmt t) = IfStmt <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (WhileStmt t) = WhileStmt <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (ReturnStmt t) = ReturnStmt <$> (annotate =<< traverse (annotateAST annotate) t)

instance AnnotateAST VarDeclaration where
  annotateAST annotate (VarDeclaration identifier expr) = VarDeclaration identifier <$> annotateAST annotate expr

instance AnnotateAST Block where
  annotateAST annotate (Block stmts) = Block <$> traverse (annotateAST annotate) stmts

instance AnnotateAST If where
  annotateAST annotate (If cond thenStmt elseStmt) = If <$> annotateAST annotate cond
                                                        <*> annotateAST annotate thenStmt
                                                        <*> traverse (annotateAST annotate) elseStmt

instance AnnotateAST While where
  annotateAST annotate (While cond body) = While <$> annotateAST annotate cond
                                                 <*> annotateAST annotate body

instance AnnotateAST Return where
  annotateAST annotate (Return tk expr) = Return tk <$> annotateAST annotate expr

instance AnnotateAST Expr where
  annotateAST annotate (UnaryExpr t)      = UnaryExpr      <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (LogicalExpr t)    = LogicalExpr    <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (BinaryExpr t)     = BinaryExpr     <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (TernaryExpr t)    = TernaryExpr    <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (GroupingExpr t)   = GroupingExpr   <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (LiteralExpr t)    = LiteralExpr    <$> annotateOpaque annotate t
  annotateAST annotate (VariableExpr t)   = VariableExpr   <$> annotateOpaque annotate t
  annotateAST annotate (AssignmentExpr t) = AssignmentExpr <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (CallExpr t)       = CallExpr       <$> (annotate =<< traverse (annotateAST annotate) t)
  annotateAST annotate (FunctionExpr t)   = FunctionExpr   <$> (annotate =<< traverse (annotateAST annotate) t)

instance AnnotateAST Unary where
  annotateAST annotate (Unary op e) = Unary op <$> annotateAST annotate e

instance AnnotateAST Logical where
  annotateAST annotate (Logical left op right) = Logical <$> annotateAST annotate left
                                                         <*> pure op
                                                         <*> annotateAST annotate right

instance AnnotateAST Binary where
  annotateAST annotate (Binary left op right) = Binary <$> annotateAST annotate left
                                                       <*> pure op
                                                       <*> annotateAST annotate right

instance AnnotateAST Ternary where
  annotateAST annotate (Ternary left op1 middle op2 right) =
    Ternary <$> annotateAST annotate left
            <*> pure op1
            <*> annotateAST annotate middle
            <*> pure op2
            <*> annotateAST annotate right

instance AnnotateAST Grouping where
  annotateAST annotate (Grouping t) = Grouping <$> annotateAST annotate t

instance AnnotateAST Assignment where
  annotateAST annotate (Assignment op e) = Assignment op <$> annotateAST annotate e

instance AnnotateAST Call where
  annotateAST annotate (Call callee paren args) = Call <$> annotateAST annotate callee
                                                       <*> pure paren
                                                       <*> traverse (annotateAST annotate) args

instance AnnotateAST Function where
  annotateAST annotate (Function tk argNames body) = Function tk argNames <$> annotateAST annotate body
