{-# LANGUAGE StrictData #-}
module HSLox.StaticAnalysis.ResolveLocals where

import Control.Carrier.State.Church (State)
import qualified Control.Carrier.State.Church as State
import Control.Effect.Writer
import Control.Monad
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified HSLox.AST as AST
import HSLox.AST.AsAST
import HSLox.AST.WalkAST
import HSLox.AST.Meta
import HSLox.ErrorReport
import HSLox.Token (Token(..))
import qualified HSLox.Util as Util

data ResolverError = ResolverError Token T.Text
  deriving (Eq, Ord, Show)

tellResolverError :: Has (Writer (Set ResolverError)) sig m => Token -> T.Text -> m ()
tellResolverError tk msg = tell . Set.singleton $ ResolverError tk msg

instance ToErrorReport ResolverError where
  toErrorReport (ResolverError token msg) =
    ErrorReport { errorReportLine = (tokenLine token)
                , errorReportWhere = (tokenLexeme token)
                , errorReportMessage = msg
                }

data ResolverMeta = ResolverMeta { resolverMetaLocalVariableScopeDistance :: Maybe Int }
  deriving (Eq, Ord, Show)

emptyResolverMeta :: ResolverMeta
emptyResolverMeta = ResolverMeta Nothing

resolveLocals :: AsIdentity f
              => Traversable f
              => Has (Writer (Set ResolverError)) sig m
              => AST.Program f
              -> m (AST.Program (WithMeta ResolverMeta f))
resolveLocals (AST.Program stmts)
  = State.evalState (emptyStack @Scope)
  $ State.evalState (emptyStack @FunctionType)
  $ AST.Program <$> (traverse (walkAST (preWalkResolvingLocals >=> preCheckForBadReturns)
                                       (postWalkResolvingLocals <=< postCheckForBadReturns))
                              stmts)

-- Catching bad returns

preCheckForBadReturns :: AsIdentity f
                      => AsAST a g
                      => Has (State (Stack FunctionType)) sig m
                      => Has (Writer (Set ResolverError)) sig m
                      => f a -> m (f a)
preCheckForBadReturns fa = do
  case content fa of
    (toFunDeclaration -> Just _) -> do
      beginFunctionType Function
    (toFunction -> Just _) -> do
      beginFunctionType Function
    (toReturn -> Just (AST.Return tk _)) -> do
      fnType <- currentFunctionType
      when (fnType == None) $
        tellResolverError tk "Cannot return from top-level code."
    _ -> pure ()
  pure fa

postCheckForBadReturns :: AsIdentity f
                       => AsAST a g
                       => Has (State (Stack FunctionType)) sig m
                       => f a -> m (f a)
postCheckForBadReturns fa = do
  case content fa of
    (toFunDeclaration -> Just _) -> do
      endFunctionType
    (toFunction -> Just _) -> do
      endFunctionType
    _ -> pure ()
  pure fa

data FunctionType = None | Function
  deriving (Eq, Ord, Show)

currentFunctionType :: Has (State (Stack FunctionType)) sig m
                    => m FunctionType
currentFunctionType = State.gets $ maybe None id . peek

beginFunctionType :: Has (State (Stack FunctionType)) sig m
                  => FunctionType -> m ()
beginFunctionType functionType = State.modify @(Stack FunctionType) $ push functionType

endFunctionType :: Has (State (Stack FunctionType)) sig m
                => m ()
endFunctionType = State.modify @(Stack FunctionType) pop_

-- Resolving local variables

preWalkResolvingLocals :: AsIdentity f
                       => AsAST a g
                       => Has (State (Stack Scope)) sig m
                       => Has (Writer (Set ResolverError)) sig m
                       => f a -> m (f a)
preWalkResolvingLocals fa = do
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

postWalkResolvingLocals :: AsIdentity f
                        => AsAST a g
                        => Has (State (Stack Scope)) sig m
                        => Has (Writer (Set ResolverError)) sig m
                        => f a -> m (WithMeta ResolverMeta f a)
postWalkResolvingLocals fa = do
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

beginFunctionScope :: Has (State (Stack Scope)) sig m
                   => Has (Writer (Set ResolverError)) sig m
                   => AST.Function f -> m ()
beginFunctionScope (AST.Function _ args _) = do
  beginScope -- args scope
  Foldable.for_ args $ \argName -> do
    declareLocal argName
    defineLocal argName
  beginScope -- body scope

endFunctionScope :: Has (State (Stack Scope)) sig m => AST.Function f -> m ()
endFunctionScope (AST.Function _ _ _) = do
  endScope -- body scope
  endScope -- args scope

newtype Scope = Scope (Map T.Text Bool)
  deriving (Show)
data BindingStatus = Missing | Declared | Defined
  deriving (Eq, Ord, Show)

emptyScope :: Scope
emptyScope = Scope Map.empty

beginScope :: Has (State (Stack Scope)) sig m => m ()
beginScope = State.modify $ push emptyScope

endScope :: Has (State (Stack Scope)) sig m => m ()
endScope = State.modify $ pop_ @Scope

declareLocal :: Has (State (Stack Scope)) sig m
             => Has (Writer (Set ResolverError)) sig m
             => Token -> m ()
declareLocal tk = Util.modifyM . overPeekA $ \s@(Scope bindings) -> do
  let name = tokenLexeme tk
  case Map.lookup name bindings of
    Nothing -> pure $ Scope (Map.insert name False bindings)
    -- avoid overwriting if it's already defined
    Just _ -> do
      tellResolverError tk "Variable with this name already declared in this scope."
      pure s

defineLocal :: Has (State (Stack Scope)) sig m => Token -> m ()
defineLocal tk = State.modify . overPeek $ \(Scope bindings) ->
  Scope (Map.insert (tokenLexeme tk) True bindings)

checkLocalIsNotBeingDeclared :: Has (State (Stack Scope)) sig m
                    => Has (Writer (Set ResolverError)) sig m
                    => Token -> m ()
checkLocalIsNotBeingDeclared token = do
  localScope <- State.gets $ peek @Scope
  case localScope of
    Just localScope
      | bindingStatus (tokenLexeme token) localScope == Declared -> do
        tellResolverError token "Cannot read local variable in its own initializer."
    _ -> pure ()

resolveLocalScopeDistance :: (Has (State (Stack Scope)) sig m)
                          => T.Text
                          -> m (Maybe Int)
resolveLocalScopeDistance name = do
  stack <- State.get @(Stack Scope)
  pure $ List.findIndex ((/= Missing) . bindingStatus name)
       . Foldable.toList
       $ stack

bindingStatus :: T.Text -> Scope -> BindingStatus
bindingStatus name (Scope bindings) =
  case Map.lookup name bindings of
    Nothing -> Missing
    Just b
      | b -> Defined
      | otherwise -> Declared

-- Stack
newtype Stack a = Stack [a]
  deriving (Show)
  deriving newtype (Foldable)

emptyStack :: Stack a
emptyStack = Stack []

push :: a -> Stack a -> Stack a
push scope (Stack scopes) = Stack (scope : scopes)

pop :: Stack a -> (Maybe a, Stack a)
pop s@(Stack []) = (Nothing, s)
pop (Stack (x:xs)) = (Just x, Stack xs)

pop_ :: Stack a -> Stack a
pop_ s@(Stack []) = s
pop_ (Stack (_:xs)) = Stack xs

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x

overPeek :: (a -> a) -> Stack a -> Stack a
overPeek _ s@(Stack []) = s
overPeek f (Stack (x : xs)) = Stack (f x : xs)

overPeekA :: Applicative m => (a -> m a) -> Stack a -> m (Stack a)
overPeekA _ s@(Stack []) = pure s
overPeekA f (Stack (x : xs)) = do
  fx <- f x
  pure $ Stack (fx : xs)
