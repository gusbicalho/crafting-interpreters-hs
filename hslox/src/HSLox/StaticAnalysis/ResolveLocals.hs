{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}

module HSLox.StaticAnalysis.ResolveLocals (
  ResolverMeta (..),
  ResolveLocalsState,
  emptyState,
  walk,
) where

import Control.Algebra (Has)
import Control.Carrier.State.Church (State)
import Control.Carrier.State.Church qualified as State
import Control.Effect.Writer (Writer)
import Control.Monad (when)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor.Const (Const (Const))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Semigroup (Last (Last))
import Data.Set (Set)
import Data.Text qualified as T
import HSLox.AST qualified as AST
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.AST.VisitAST (visit_, visitor)
import HSLox.AST.WalkAST (Walker (Walker))
import HSLox.AST.WalkAST qualified as WalkAST
import HSLox.StaticAnalysis.Error (
  AnalysisError,
  tellAnalysisError,
 )
import HSLox.StaticAnalysis.Stack (Stack)
import HSLox.StaticAnalysis.Stack qualified as Stack
import HSLox.Token (Token (..))
import HSLox.Util qualified as Util

walk ::
  Has (State ResolveLocalsState) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WalkAST.Walker input input interOut (ResolverMeta, interOut) m
walk = Walker preResolvingLocals postResolvingLocals

preResolvingLocals ::
  Has (State ResolveLocalsState) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WalkAST.Walk meta meta m
preResolvingLocals fa = do
  AST.Meta.content fa
    & visit_
      [ visitor \AST.Block{} -> do
          beginScope
      , visitor \(AST.VarDeclaration tk _) -> do
          declareLocal tk
      , visitor \(AST.ClassDeclaration tk superclass _) -> do
          declareLocal tk
          defineLocal (tokenLexeme tk)
          when (isJust superclass) $ do
            beginScope -- super
            defineLocal "super"
          beginScope -- instance
          defineLocal "this"
      , visitor \(AST.FunDeclaration _ fn) -> do
          beginFunctionScope fn
      , visitor \fn@AST.Function{} -> do
          beginFunctionScope fn
      ]
  pure fa

postResolvingLocals ::
  Has (State ResolveLocalsState) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WalkAST.Walk meta (ResolverMeta, meta) m
postResolvingLocals fa = do
  meta <-
    AST.Meta.content fa
      & visit_
        [ visitor \AST.Block{} -> do
            endScope
            pure mempty
        , visitor \(AST.VarDeclaration tk _) -> do
            defineLocal (tokenLexeme tk)
            pure mempty
        , visitor \(AST.ClassDeclaration _ superclass _) -> do
            endScope -- class
            when
              (isJust superclass)
              endScope -- super
            pure mempty
        , visitor \(AST.FunDeclaration tk fn) -> do
            endFunctionScope fn
            declareLocal tk
            defineLocal (tokenLexeme tk)
            pure mempty
        , visitor \fn@AST.Function{} -> do
            endFunctionScope fn
            pure mempty
        , visitor \(Const (AST.Variable tk)) -> do
            checkLocalIsNotBeingDeclared tk
            distance <- resolveLocalScopeDistance (tokenLexeme tk)
            pure $ mempty{resolverMetaLocalVariableScopeDistance = distance}
        , visitor \(AST.Assignment tk _) -> do
            checkLocalIsNotBeingDeclared tk
            distance <- resolveLocalScopeDistance (tokenLexeme tk)
            pure $ mempty{resolverMetaLocalVariableScopeDistance = distance}
        , visitor \(Const (AST.This tk)) -> do
            distance <- resolveLocalScopeDistance (tokenLexeme tk)
            pure $ mempty{resolverMetaLocalVariableScopeDistance = distance}
        , visitor \(Const (AST.Super keywordTk _)) -> do
            distance <- resolveLocalScopeDistance (tokenLexeme keywordTk)
            pure $ mempty{resolverMetaLocalVariableScopeDistance = distance}
        ]
  pure $ AST.Meta.addMetaItem meta fa

newtype ResolveLocalsState = RLS {getStack :: Stack Scope}

emptyState :: ResolveLocalsState
emptyState = RLS Stack.emptyStack

overStack :: (Stack Scope -> Stack Scope) -> ResolveLocalsState -> ResolveLocalsState
overStack f rls@(RLS stack) = rls{getStack = f stack}

overStackF :: Functor f => (Stack Scope -> f (Stack Scope)) -> ResolveLocalsState -> f ResolveLocalsState
overStackF f rls@(RLS stack) = (\newStack -> rls{getStack = newStack}) <$> f stack

newtype ResolverMeta = ResolverMeta {resolverMetaLocalVariableScopeDistance :: Maybe Int}
  deriving stock (Eq, Ord, Show)
  deriving (Monoid, Semigroup) via (Maybe (Last Int))

beginFunctionScope ::
  Has (State ResolveLocalsState) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  AST.Function f ->
  m ()
beginFunctionScope (AST.Function _ funRecId args _) = do
  beginScope -- args scope
  case funRecId of
    Just funRecId -> declareLocal funRecId
    Nothing -> pure ()
  Foldable.for_ args $ \argName -> do
    declareLocal argName
    defineLocal (tokenLexeme argName)
  beginScope -- body scope

endFunctionScope :: Has (State ResolveLocalsState) sig m => AST.Function f -> m ()
endFunctionScope AST.Function{} = do
  endScope -- body scope
  endScope -- args scope

newtype Scope = Scope (Map T.Text Bool)
  deriving stock (Show)
data BindingStatus = Missing | Declared | Defined
  deriving stock (Eq, Ord, Show)

emptyScope :: Scope
emptyScope = Scope Map.empty

beginScope :: Has (State ResolveLocalsState) sig m => m ()
beginScope = State.modify . overStack $ Stack.push emptyScope

endScope :: Has (State ResolveLocalsState) sig m => m ()
endScope = State.modify . overStack $ Stack.pop_ @Scope

declareLocal ::
  Has (State ResolveLocalsState) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  Token ->
  m ()
declareLocal tk = Util.modifyM . overStackF . Stack.overPeekA $ \s@(Scope bindings) -> do
  let name = tokenLexeme tk
  case Map.lookup name bindings of
    Nothing -> pure $ Scope (Map.insert name False bindings)
    -- avoid overwriting if it's already defined
    Just _ -> do
      tellAnalysisError tk "Variable with this name already declared in this scope."
      pure s

defineLocal :: Has (State ResolveLocalsState) sig m => T.Text -> m ()
defineLocal name = State.modify . overStack . Stack.overPeek $ \(Scope bindings) ->
  Scope (Map.insert name True bindings)

checkLocalIsNotBeingDeclared ::
  Has (State ResolveLocalsState) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  Token ->
  m ()
checkLocalIsNotBeingDeclared token = do
  localScope <- State.gets $ Stack.peek @Scope . getStack
  case localScope of
    Just localScope
      | bindingStatus (tokenLexeme token) localScope == Declared -> do
        tellAnalysisError token "Cannot read local variable in its own initializer."
    _ -> pure ()

resolveLocalScopeDistance ::
  (Has (State ResolveLocalsState) sig m) =>
  T.Text ->
  m (Maybe Int)
resolveLocalScopeDistance name = do
  state <- State.get @ResolveLocalsState
  pure $
    List.findIndex ((/= Missing) . bindingStatus name)
      . Foldable.toList
      . getStack
      $ state

bindingStatus :: T.Text -> Scope -> BindingStatus
bindingStatus name (Scope bindings) =
  case Map.lookup name bindings of
    Nothing -> Missing
    Just b
      | b -> Defined
      | otherwise -> Declared
