module HSLox.StaticAnalysis.ResolveLocals
  ( ResolverMeta (..), ResolveLocalsState
  , emptyState
  , preResolvingLocals
  , postResolvingLocals
  ) where

import Control.Carrier.State.Church (State)
import qualified Control.Carrier.State.Church as State
import Control.Effect.Writer
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Text as T
import qualified HSLox.AST as AST
import HSLox.AST.AsAST
import HSLox.AST.Meta
import HSLox.StaticAnalysis.Error
import HSLox.StaticAnalysis.Stack (Stack)
import qualified HSLox.StaticAnalysis.Stack as Stack
import HSLox.Token (Token(..))
import qualified HSLox.Util as Util

preResolvingLocals :: AsIdentity f
                       => AsAST a g
                       => Has (State ResolveLocalsState) sig m
                       => Has (Writer (Set AnalysisError)) sig m
                       => f a -> m (f a)
preResolvingLocals fa = do
  case content fa of
    (toBlock -> Just _) -> do
      beginScope
    (toVarDeclaration -> Just (AST.VarDeclaration tk _)) -> do
      declareLocal tk
    (toFunDeclaration -> Just (AST.FunDeclaration tk fn)) -> do
      declareLocal tk
      defineLocal tk
      beginFunctionScope fn
    (toFunction -> Just fn) -> do
      beginFunctionScope fn
    _ -> pure ()
  pure fa

postResolvingLocals :: AsIdentity f
                        => AsAST a g
                        => Has (State ResolveLocalsState) sig m
                        => Has (Writer (Set AnalysisError)) sig m
                        => f a -> m (WithMeta ResolverMeta f a)
postResolvingLocals fa = do
  meta <- case content fa of
    (toBlock -> Just _) -> do
      endScope
      pure emptyResolverMeta
    (toVarDeclaration -> Just (AST.VarDeclaration tk _)) -> do
      defineLocal tk
      pure emptyResolverMeta
    (toFunDeclaration -> Just (AST.FunDeclaration _ fn)) -> do
      endFunctionScope fn
      pure emptyResolverMeta
    (toFunction -> Just fn) -> do
      endFunctionScope fn
      pure emptyResolverMeta
    (toVariable -> Just (AST.Variable tk)) -> do
      checkLocalIsNotBeingDeclared tk
      distance <- resolveLocalScopeDistance (tokenLexeme tk)
      pure $ emptyResolverMeta { resolverMetaLocalVariableScopeDistance = distance }
    (toAssignment -> Just (AST.Assignment tk _)) -> do
      checkLocalIsNotBeingDeclared tk
      distance <- resolveLocalScopeDistance (tokenLexeme tk)
      pure $ emptyResolverMeta { resolverMetaLocalVariableScopeDistance = distance }
    _ -> pure emptyResolverMeta
  pure $ withMeta meta fa

newtype ResolveLocalsState = RLS { getStack :: Stack Scope }

emptyState :: ResolveLocalsState
emptyState = RLS Stack.emptyStack

overStack :: (Stack Scope -> Stack Scope) -> ResolveLocalsState -> ResolveLocalsState
overStack f rls@(RLS stack) = rls { getStack = f stack }

overStackF :: Functor f => (Stack Scope -> f (Stack Scope)) -> ResolveLocalsState -> f ResolveLocalsState
overStackF f rls@(RLS stack) = (\newStack -> rls { getStack = newStack }) <$> f stack

data ResolverMeta = ResolverMeta { resolverMetaLocalVariableScopeDistance :: Maybe Int }
  deriving (Eq, Ord, Show)

emptyResolverMeta :: ResolverMeta
emptyResolverMeta = ResolverMeta Nothing

beginFunctionScope :: Has (State ResolveLocalsState) sig m
                   => Has (Writer (Set AnalysisError)) sig m
                   => AST.Function f -> m ()
beginFunctionScope (AST.Function _ args _) = do
  beginScope -- args scope
  Foldable.for_ args $ \argName -> do
    declareLocal argName
    defineLocal argName
  beginScope -- body scope

endFunctionScope :: Has (State ResolveLocalsState) sig m => AST.Function f -> m ()
endFunctionScope (AST.Function _ _ _) = do
  endScope -- body scope
  endScope -- args scope

newtype Scope = Scope (Map T.Text Bool)
  deriving (Show)
data BindingStatus = Missing | Declared | Defined
  deriving (Eq, Ord, Show)

emptyScope :: Scope
emptyScope = Scope Map.empty

beginScope :: Has (State ResolveLocalsState) sig m => m ()
beginScope = State.modify . overStack $ Stack.push emptyScope

endScope :: Has (State ResolveLocalsState) sig m => m ()
endScope = State.modify . overStack $ Stack.pop_ @Scope

declareLocal :: Has (State ResolveLocalsState) sig m
             => Has (Writer (Set AnalysisError)) sig m
             => Token -> m ()
declareLocal tk = Util.modifyM . overStackF . Stack.overPeekA $ \s@(Scope bindings) -> do
  let name = tokenLexeme tk
  case Map.lookup name bindings of
    Nothing -> pure $ Scope (Map.insert name False bindings)
    -- avoid overwriting if it's already defined
    Just _ -> do
      tellAnalysisError tk "Variable with this name already declared in this scope."
      pure s

defineLocal :: Has (State ResolveLocalsState) sig m => Token -> m ()
defineLocal tk = State.modify . overStack . Stack.overPeek $ \(Scope bindings) ->
  Scope (Map.insert (tokenLexeme tk) True bindings)

checkLocalIsNotBeingDeclared :: Has (State ResolveLocalsState) sig m
                    => Has (Writer (Set AnalysisError)) sig m
                    => Token -> m ()
checkLocalIsNotBeingDeclared token = do
  localScope <- State.gets $ Stack.peek @Scope . getStack
  case localScope of
    Just localScope
      | bindingStatus (tokenLexeme token) localScope == Declared -> do
        tellAnalysisError token "Cannot read local variable in its own initializer."
    _ -> pure ()

resolveLocalScopeDistance :: (Has (State ResolveLocalsState) sig m)
                          => T.Text
                          -> m (Maybe Int)
resolveLocalScopeDistance name = do
  state <- State.get @ResolveLocalsState
  pure $ List.findIndex ((/= Missing) . bindingStatus name)
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
