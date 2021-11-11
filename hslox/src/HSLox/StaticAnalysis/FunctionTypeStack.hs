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
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import HSLox.AST qualified as AST
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.AST.VisitAST (visit, visit_, visitor)
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
  WalkAST.Walk meta meta m
preFunctionTypeStack fa = do
  AST.Meta.content fa
    & visit_
      [ visitor $ \AST.FunDeclaration{} -> do
          beginFunctionType Function
      , visitor $ \AST.ClassDeclaration{} -> do
          beginFunctionType Class
      , visitor $ \(AST.Function tk _ _ _) -> do
          current <- currentFunctionType
          case current of
            Class
              | tokenLexeme tk == "init" -> beginFunctionType Initializer
              | otherwise -> beginFunctionType Method
            _ -> beginFunctionType Function
      ]
  pure fa

postFunctionTypeStack ::
  Has (State FunctionTypeStack) sig m =>
  WalkAST.Walk meta meta m
postFunctionTypeStack fa = do
  AST.Meta.content fa
    & visit
      (pure ())
      [ visitor $ \AST.FunDeclaration{} -> do
          endFunctionType
      , visitor $ \AST.ClassDeclaration{} -> do
          endFunctionType
      , visitor $ \AST.Function{} -> do
          endFunctionType
      ]
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
