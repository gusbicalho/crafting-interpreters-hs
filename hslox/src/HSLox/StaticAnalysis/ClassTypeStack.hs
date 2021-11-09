module HSLox.StaticAnalysis.ClassTypeStack (
  ClassTypeStack,
  ClassType (..),
  emptyState,
  preClassTypeStack,
  postClassTypeStack,
  currentClassType,
) where

import Control.Algebra (Has)
import Control.Carrier.State.Church (State)
import Control.Carrier.State.Church qualified as State
import Data.Maybe (fromMaybe, isJust)
import HSLox.AST qualified as AST
import HSLox.AST.AsAST (AsAST (..))
import HSLox.AST.Meta (WithMeta)
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.StaticAnalysis.Stack (Stack)
import HSLox.StaticAnalysis.Stack qualified as Stack

preClassTypeStack ::
  AsAST a g =>
  Has (State ClassTypeStack) sig m =>
  WithMeta meta a ->
  m (WithMeta meta a)
preClassTypeStack fa = do
  case AST.Meta.content fa of
    (toClassDeclaration -> Just (AST.ClassDeclaration _ super _)) -> do
      beginClassType $
        if isJust super
          then Subclass
          else Class
    _ -> pure ()
  pure fa

postClassTypeStack ::
  AsAST a g =>
  Has (State ClassTypeStack) sig m =>
  WithMeta meta a ->
  m (WithMeta meta a)
postClassTypeStack fa = do
  case AST.Meta.content fa of
    (toClassDeclaration -> Just _) -> do
      endClassType
    _ -> pure ()
  pure fa

newtype ClassTypeStack = CTS {getStack :: Stack ClassType}

emptyState :: ClassTypeStack
emptyState = CTS Stack.emptyStack

overStack :: (Stack ClassType -> Stack ClassType) -> ClassTypeStack -> ClassTypeStack
overStack f rls@(CTS stack) = rls{getStack = f stack}

data ClassType = None | Class | Subclass
  deriving stock (Eq, Ord, Show)

currentClassType ::
  Has (State ClassTypeStack) sig m =>
  m ClassType
currentClassType = State.gets $ fromMaybe None . Stack.peek . getStack

beginClassType ::
  Has (State ClassTypeStack) sig m =>
  ClassType ->
  m ()
beginClassType functionType =
  State.modify @ClassTypeStack
    . overStack
    $ Stack.push functionType

endClassType ::
  Has (State ClassTypeStack) sig m =>
  m ()
endClassType =
  State.modify @ClassTypeStack
    . overStack
    $ Stack.pop_
