{-# LANGUAGE DerivingVia #-}

module HSLox.AST.VisitAST (
  Visitor,
  VisitAST,
  visitor,
  visit,
  visit_,
  visitOnly,
  visitOnly_,
  pick,
  Const (..),
) where

import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Functor.Const (Const (..))
import Data.Semigroup (Last (Last))
import GHC.Generics (Generic)
import Generic.Data (Generically (Generically))
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

data Visitor meta r = Visitor
  { onExpr :: Maybe (Last (Expr meta -> r))
  , onStmt :: Maybe (Last (Stmt meta -> r))
  , onVarDeclaration :: Maybe (Last (VarDeclaration meta -> r))
  , onFunDeclaration :: Maybe (Last (FunDeclaration meta -> r))
  , onClassDeclaration :: Maybe (Last (ClassDeclaration meta -> r))
  , onBlock :: Maybe (Last (Block meta -> r))
  , onIf :: Maybe (Last (If meta -> r))
  , onWhile :: Maybe (Last (While meta -> r))
  , onReturn :: Maybe (Last (Return meta -> r))
  , onUnary :: Maybe (Last (Unary meta -> r))
  , onLogical :: Maybe (Last (Logical meta -> r))
  , onBinary :: Maybe (Last (Binary meta -> r))
  , onTernary :: Maybe (Last (Ternary meta -> r))
  , onGrouping :: Maybe (Last (Grouping meta -> r))
  , onAssignment :: Maybe (Last (Assignment meta -> r))
  , onCall :: Maybe (Last (Call meta -> r))
  , onGetProperty :: Maybe (Last (GetProperty meta -> r))
  , onSetProperty :: Maybe (Last (SetProperty meta -> r))
  , onFunction :: Maybe (Last (Function meta -> r))
  , onVariable :: Maybe (Last (Variable -> r))
  , onLiteral :: Maybe (Last (Literal -> r))
  , onThis :: Maybe (Last (This -> r))
  , onSuper :: Maybe (Last (Super -> r))
  }
  deriving stock (Generic)
  deriving (Monoid, Semigroup) via (Generically (Visitor meta r))

visit :: forall ast meta r. VisitAST ast => r -> [Visitor meta r] -> ast meta -> r
visit orElse visitor ast =
  case getVisitFn @ast (fold visitor) of
    Just visit -> visit ast
    Nothing -> orElse

visit_ ::
  ( VisitAST ast
  , Applicative m
  , Monoid result
  ) =>
  [Visitor meta (m result)] ->
  ast meta ->
  m result
visit_ = visit (pure mempty)

-- visitOnly :: VisitAST ast => r -> (ast meta -> r) -> ast meta -> r
visitOnly ::
  forall acceptableAST meta r.
  VisitAST acceptableAST =>
  r ->
  (acceptableAST meta -> r) ->
  forall ast.
  VisitAST ast =>
  ast meta ->
  r
visitOnly orElse visitFn = visit orElse [visitor visitFn]

visitOnly_ ::
  forall acceptableAST meta m result.
  ( VisitAST acceptableAST
  , Applicative m
  , Monoid result
  ) =>
  (acceptableAST meta -> m result) ->
  forall ast.
  VisitAST ast =>
  ast meta ->
  m result
visitOnly_ = visitOnly (pure mempty)

pick ::
  forall acceptableAST meta.
  VisitAST acceptableAST =>
  forall ast.
  VisitAST ast =>
  ast meta ->
  Maybe (acceptableAST meta)
pick = visitOnly Nothing Just

class VisitAST a where
  visitor :: (a meta -> r) -> Visitor meta r
  getVisitFn :: Visitor meta r -> Maybe (a meta -> r)

instance VisitAST Stmt where
  visitor visitFn = mempty{onStmt = Just $ Last visitFn}
  getVisitFn = coerce . onStmt
instance VisitAST VarDeclaration where
  visitor visitFn = mempty{onVarDeclaration = Just $ Last visitFn}
  getVisitFn = coerce . onVarDeclaration
instance VisitAST FunDeclaration where
  visitor visitFn = mempty{onFunDeclaration = Just $ Last visitFn}
  getVisitFn = coerce . onFunDeclaration
instance VisitAST ClassDeclaration where
  visitor visitFn = mempty{onClassDeclaration = Just $ Last visitFn}
  getVisitFn = coerce . onClassDeclaration
instance VisitAST Block where
  visitor visitFn = mempty{onBlock = Just $ Last visitFn}
  getVisitFn = coerce . onBlock
instance VisitAST If where
  visitor visitFn = mempty{onIf = Just $ Last visitFn}
  getVisitFn = coerce . onIf
instance VisitAST While where
  visitor visitFn = mempty{onWhile = Just $ Last visitFn}
  getVisitFn = coerce . onWhile
instance VisitAST Return where
  visitor visitFn = mempty{onReturn = Just $ Last visitFn}
  getVisitFn = coerce . onReturn
instance VisitAST Expr where
  visitor visitFn = mempty{onExpr = Just $ Last visitFn}
  getVisitFn = coerce . onExpr
instance VisitAST Unary where
  visitor visitFn = mempty{onUnary = Just $ Last visitFn}
  getVisitFn = coerce . onUnary
instance VisitAST Logical where
  visitor visitFn = mempty{onLogical = Just $ Last visitFn}
  getVisitFn = coerce . onLogical
instance VisitAST Binary where
  visitor visitFn = mempty{onBinary = Just $ Last visitFn}
  getVisitFn = coerce . onBinary
instance VisitAST Ternary where
  visitor visitFn = mempty{onTernary = Just $ Last visitFn}
  getVisitFn = coerce . onTernary
instance VisitAST Grouping where
  visitor visitFn = mempty{onGrouping = Just $ Last visitFn}
  getVisitFn = coerce . onGrouping
instance VisitAST Assignment where
  visitor visitFn = mempty{onAssignment = Just $ Last visitFn}
  getVisitFn = coerce . onAssignment
instance VisitAST Call where
  visitor visitFn = mempty{onCall = Just $ Last visitFn}
  getVisitFn = coerce . onCall
instance VisitAST GetProperty where
  visitor visitFn = mempty{onGetProperty = Just $ Last visitFn}
  getVisitFn = coerce . onGetProperty
instance VisitAST SetProperty where
  visitor visitFn = mempty{onSetProperty = Just $ Last visitFn}
  getVisitFn = coerce . onSetProperty
instance VisitAST Function where
  visitor visitFn = mempty{onFunction = Just $ Last visitFn}
  getVisitFn = coerce . onFunction

instance VisitAST (Const Variable) where
  visitor visitFn = mempty{onVariable = Just $ Last (visitFn . Const)}
  getVisitFn = coerce . onVariable
instance VisitAST (Const Literal) where
  visitor visitFn = mempty{onLiteral = Just $ Last (visitFn . Const)}
  getVisitFn = coerce . onLiteral
instance VisitAST (Const This) where
  visitor visitFn = mempty{onThis = Just $ Last (visitFn . Const)}
  getVisitFn = coerce . onThis
instance VisitAST (Const Super) where
  visitor visitFn = mempty{onSuper = Just $ Last (visitFn . Const)}
  getVisitFn = coerce . onSuper
