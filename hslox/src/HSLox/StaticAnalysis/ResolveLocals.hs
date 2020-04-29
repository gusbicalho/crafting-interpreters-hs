{-# LANGUAGE StrictData #-}
module HSLox.StaticAnalysis.ResolveLocals where

import Control.Carrier.State.Church (State)
import qualified Control.Carrier.State.Church as State
import Control.Effect.Writer
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import HSLox.AST
import HSLox.AST.AsAST
import HSLox.AST.WalkAST
import HSLox.AST.Meta
import HSLox.ErrorReport
import HSLox.Token (Token(..))

data ResolverError = ResolverError Token T.Text
  deriving (Eq, Ord, Show)

tellResolverError :: Has (Writer (Set ResolverError)) sig m => Token -> T.Text -> m ()
tellResolverError tk msg = tell . Set.singleton $ ResolverError tk msg

instance ToErrorReport ResolverError where
  toErrorReport (ResolverError token msg) =
    ErrorReport { errorReportLine = (tokenLine token)
                , errorReportWhere = ""
                , errorReportMessage = msg
                }

data ResolverMeta = ResolverMeta { resolverMetaLocalVariableScopeDistance :: Maybe Int }
  deriving (Eq, Ord, Show)

emptyResolverMeta :: ResolverMeta
emptyResolverMeta = ResolverMeta Nothing

resolveLocals :: AsIdentity f
              => Traversable f
              => Has (Writer (Set ResolverError)) sig m
              => Program f
              -> m (Program (WithMeta ResolverMeta f))
resolveLocals (Program stmts)
  = State.evalState (emptyStack @Scope)
  $ Program <$> (traverse (walkAST preWalk postWalk) stmts)
  where
    preWalk fa = do
      let node = content fa
      case node of
        (toBlock -> Just _) -> do
          beginScope
        (toVarDeclaration -> Just (VarDeclaration tk _)) -> do
          declareM (tokenLexeme tk)
        (toFunDeclaration -> Just (FunDeclaration tk (Function _ args _))) -> do
          declareM (tokenLexeme tk)
          defineM (tokenLexeme tk)
          beginScope -- args scope
          Foldable.for_ args $ \(tokenLexeme -> argName) -> do
            declareM argName
            defineM argName
          beginScope -- body scope
        (toFunction -> Just (Function _ args _)) -> do
          beginScope -- args scope
          Foldable.for_ args $ \(tokenLexeme -> argName) -> do
            declareM argName
            defineM argName
          beginScope -- body scope
        _ -> pure ()
      pure fa
    postWalk fa = do
      let node = content fa
      meta <- case node of
        (toBlock -> Just _) -> do
          endScope
          pure emptyResolverMeta
        (toVarDeclaration -> Just (VarDeclaration tk _)) -> do
          defineM (tokenLexeme tk)
          pure emptyResolverMeta
        (toFunDeclaration -> Just _) -> do
          endScope -- body scope
          endScope -- args scope
          pure emptyResolverMeta
        (toFunction -> Just _) -> do
          endScope -- body scope
          endScope -- args scope
          pure emptyResolverMeta
        (toVariable -> Just (Variable tk)) -> do
          checkLocalIsNotBeingDeclared tk
          distance <- resolveLocalScopeDistance (tokenLexeme tk)
          pure $ emptyResolverMeta { resolverMetaLocalVariableScopeDistance = distance }
        (toAssignment -> Just (Assignment tk _)) -> do
          checkLocalIsNotBeingDeclared tk
          distance <- resolveLocalScopeDistance (tokenLexeme tk)
          pure $ emptyResolverMeta { resolverMetaLocalVariableScopeDistance = distance }
        _ -> pure emptyResolverMeta
      pure $ withMeta meta fa

newtype Scope = Scope (Map T.Text Bool)
  deriving (Show)
data BindingStatus = Missing | Declared | Defined
  deriving (Eq, Ord, Show)

emptyScope :: Scope
emptyScope = Scope Map.empty

declare :: T.Text -> Scope -> Scope
declare name s@(Scope bindings) =
  case Map.lookup name bindings of
    Nothing -> Scope (Map.insert name False bindings)
    -- avoid overwriting if it's already defined
    Just _ -> s

define :: T.Text -> Scope -> Scope
define name (Scope bindings) = Scope (Map.insert name True bindings)

beginScope :: Has (State (Stack Scope)) sig m => m ()
beginScope = State.modify $ push emptyScope

endScope :: Has (State (Stack Scope)) sig m => m ()
endScope = State.modify $ pop_ @Scope

declareM :: Has (State (Stack Scope)) sig m => T.Text -> m ()
declareM name = State.modify . overPeek $ declare name

defineM :: Has (State (Stack Scope)) sig m => T.Text -> m ()
defineM name = State.modify . overPeek $ define name

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
