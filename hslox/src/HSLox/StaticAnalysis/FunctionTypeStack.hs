module HSLox.StaticAnalysis.FunctionTypeStack (
  FunctionTypeStack,
  FunctionType (..),
  emptyState,
  walk,
  currentFunctionType,
) where

import Control.Algebra (Has)
import Control.Carrier.State.Church (State)
import Control.Carrier.State.Church qualified as State
import Data.Maybe (fromMaybe)
import HSLox.AST qualified as AST
import HSLox.AST.AsAST (AsAST (..))
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.AST.WalkAST qualified as WalkAST
import HSLox.StaticAnalysis.Stack (Stack)
import HSLox.StaticAnalysis.Stack qualified as Stack
import HSLox.Token (Token (..))

walk ::
  Has (State FunctionTypeStack) sig m =>
  WalkAST.NeutralWalker input output m
walk = WalkAST.Walker preFunctionTypeStack postFunctionTypeStack

preFunctionTypeStack ::
  Has (State FunctionTypeStack) sig m =>
  WalkAST.PreWalk meta meta m
preFunctionTypeStack fa = do
  case AST.Meta.content fa of
    (toFunDeclaration -> Just _) -> do
      beginFunctionType Function
    (toClassDeclaration -> Just _) -> do
      beginFunctionType Class
    (toFunction -> Just (AST.Function tk _ _ _)) -> do
      current <- currentFunctionType
      case current of
        Class
          | tokenLexeme tk == "init" -> beginFunctionType Initializer
          | otherwise -> beginFunctionType Method
        _ -> beginFunctionType Function
    _ -> pure ()
  pure fa

postFunctionTypeStack ::
  Has (State FunctionTypeStack) sig m =>
  WalkAST.PostWalk meta meta m
postFunctionTypeStack fa = do
  case AST.Meta.content fa of
    (toFunDeclaration -> Just _) -> do
      endFunctionType
    (toClassDeclaration -> Just _) -> do
      endFunctionType
    (toFunction -> Just _) -> do
      endFunctionType
    _ -> pure ()
  pure fa

newtype FunctionTypeStack = FTS {getStack :: Stack FunctionType}

emptyState :: FunctionTypeStack
emptyState = FTS Stack.emptyStack

overStack :: (Stack FunctionType -> Stack FunctionType) -> FunctionTypeStack -> FunctionTypeStack
overStack f rls@(FTS stack) = rls{getStack = f stack}

data FunctionType = None | Function | Class | Method | Initializer
  deriving stock (Eq, Ord, Show)

currentFunctionType ::
  Has (State FunctionTypeStack) sig m =>
  m FunctionType
currentFunctionType = State.gets $ fromMaybe None . Stack.peek . getStack

beginFunctionType ::
  Has (State FunctionTypeStack) sig m =>
  FunctionType ->
  m ()
beginFunctionType functionType =
  State.modify @FunctionTypeStack
    . overStack
    $ Stack.push functionType

endFunctionType ::
  Has (State FunctionTypeStack) sig m =>
  m ()
endFunctionType =
  State.modify @FunctionTypeStack
    . overStack
    $ Stack.pop_
