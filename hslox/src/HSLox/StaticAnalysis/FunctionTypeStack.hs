module HSLox.StaticAnalysis.FunctionTypeStack
  ( FunctionTypeStack, FunctionType (..)
  , emptyState
  , preFunctionTypeStack
  , postFunctionTypeStack
  , currentFunctionType
  ) where


import Control.Carrier.State.Church (State)
import qualified Control.Carrier.State.Church as State
import Control.Effect.Writer
import qualified HSLox.AST as AST
import HSLox.AST.AsAST
import HSLox.AST.Meta
import HSLox.StaticAnalysis.Stack (Stack)
import qualified HSLox.StaticAnalysis.Stack as Stack
import HSLox.Token (Token (..))

preFunctionTypeStack :: AsIdentity f
                     => AsAST a g
                     => Has (State FunctionTypeStack) sig m
                     => f a -> m (f a)
preFunctionTypeStack fa = do
  case content fa of
    (toFunDeclaration -> Just _) -> do
      beginFunctionType Function
    (toClassDeclaration -> Just _) -> do
      beginFunctionType Class
    (toFunction -> Just (AST.Function tk _ _)) -> do
      current <- currentFunctionType
      case current of
        Class
          | tokenLexeme tk == "init" -> beginFunctionType Initializer
          | otherwise -> beginFunctionType Method
        _     -> beginFunctionType Function
    _ -> pure ()
  pure fa

postFunctionTypeStack :: AsIdentity f
                      => AsAST a g
                      => Has (State FunctionTypeStack) sig m
                      => f a -> m (f a)
postFunctionTypeStack fa = do
  case content fa of
    (toFunDeclaration -> Just _) -> do
      endFunctionType
    (toClassDeclaration -> Just _) -> do
      endFunctionType
    (toFunction -> Just _) -> do
      endFunctionType
    _ -> pure ()
  pure fa

newtype FunctionTypeStack = FTS { getStack :: Stack FunctionType }

emptyState :: FunctionTypeStack
emptyState = FTS Stack.emptyStack

overStack :: (Stack FunctionType -> Stack FunctionType) -> FunctionTypeStack -> FunctionTypeStack
overStack f rls@(FTS stack) = rls { getStack = f stack }

data FunctionType = None | Function | Class | Method | Initializer
  deriving (Eq, Ord, Show)

currentFunctionType :: Has (State FunctionTypeStack) sig m
                    => m FunctionType
currentFunctionType = State.gets $ maybe None id . Stack.peek . getStack

beginFunctionType :: Has (State FunctionTypeStack) sig m
                  => FunctionType -> m ()
beginFunctionType functionType = State.modify @FunctionTypeStack
                               . overStack
                               $ Stack.push functionType

endFunctionType :: Has (State FunctionTypeStack) sig m
                => m ()
endFunctionType = State.modify @FunctionTypeStack
                . overStack
                $ Stack.pop_
