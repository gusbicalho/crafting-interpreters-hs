module HSLox.StaticAnalysis.CheckBadReturns
  ( CheckBadReturnsState
  , emptyState
  , preCheckForBadReturns
  , postCheckForBadReturns
  ) where


import Control.Carrier.State.Church (State)
import qualified Control.Carrier.State.Church as State
import Control.Effect.Writer
import Control.Monad
import Data.Set (Set)
import qualified HSLox.AST as AST
import HSLox.AST.AsAST
import HSLox.AST.Meta
import HSLox.StaticAnalysis.Error
import HSLox.StaticAnalysis.Stack (Stack)
import qualified HSLox.StaticAnalysis.Stack as Stack

preCheckForBadReturns :: AsIdentity f
                      => AsAST a g
                      => Has (State CheckBadReturnsState) sig m
                      => Has (Writer (Set AnalysisError)) sig m
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
        tellAnalysisError tk "Cannot return from top-level code."
    _ -> pure ()
  pure fa

postCheckForBadReturns :: AsIdentity f
                       => AsAST a g
                       => Has (State CheckBadReturnsState) sig m
                       => f a -> m (f a)
postCheckForBadReturns fa = do
  case content fa of
    (toFunDeclaration -> Just _) -> do
      endFunctionType
    (toFunction -> Just _) -> do
      endFunctionType
    _ -> pure ()
  pure fa

newtype CheckBadReturnsState = CBRS { getStack :: Stack FunctionType }

emptyState :: CheckBadReturnsState
emptyState = CBRS Stack.emptyStack

overStack :: (Stack FunctionType -> Stack FunctionType) -> CheckBadReturnsState -> CheckBadReturnsState
overStack f rls@(CBRS stack) = rls { getStack = f stack }

data FunctionType = None | Function
  deriving (Eq, Ord, Show)

currentFunctionType :: Has (State CheckBadReturnsState) sig m
                    => m FunctionType
currentFunctionType = State.gets $ maybe None id . Stack.peek . getStack

beginFunctionType :: Has (State CheckBadReturnsState) sig m
                  => FunctionType -> m ()
beginFunctionType functionType = State.modify @CheckBadReturnsState
                               . overStack
                               $ Stack.push functionType

endFunctionType :: Has (State CheckBadReturnsState) sig m
                => m ()
endFunctionType = State.modify @CheckBadReturnsState
                . overStack
                $ Stack.pop_
