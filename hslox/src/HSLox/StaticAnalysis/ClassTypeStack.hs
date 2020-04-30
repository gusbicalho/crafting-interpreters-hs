module HSLox.StaticAnalysis.ClassTypeStack
  ( ClassTypeStack, ClassType (..)
  , emptyState
  , preClassTypeStack
  , postClassTypeStack
  , currentClassType
  ) where


import Control.Carrier.State.Church (State)
import qualified Control.Carrier.State.Church as State
import Control.Effect.Writer
import HSLox.AST.AsAST
import HSLox.AST.Meta
import HSLox.StaticAnalysis.Stack (Stack)
import qualified HSLox.StaticAnalysis.Stack as Stack

preClassTypeStack :: AsIdentity f
                     => AsAST a g
                     => Has (State ClassTypeStack) sig m
                     => f a -> m (f a)
preClassTypeStack fa = do
  case content fa of
    (toClassDeclaration -> Just _) -> do
      beginClassType Class
    _ -> pure ()
  pure fa

postClassTypeStack :: AsIdentity f
                      => AsAST a g
                      => Has (State ClassTypeStack) sig m
                      => f a -> m (f a)
postClassTypeStack fa = do
  case content fa of
    (toClassDeclaration -> Just _) -> do
      endClassType
    _ -> pure ()
  pure fa

newtype ClassTypeStack = CTS { getStack :: Stack ClassType }

emptyState :: ClassTypeStack
emptyState = CTS Stack.emptyStack

overStack :: (Stack ClassType -> Stack ClassType) -> ClassTypeStack -> ClassTypeStack
overStack f rls@(CTS stack) = rls { getStack = f stack }

data ClassType = None | Class
  deriving (Eq, Ord, Show)

currentClassType :: Has (State ClassTypeStack) sig m
                    => m ClassType
currentClassType = State.gets $ maybe None id . Stack.peek . getStack

beginClassType :: Has (State ClassTypeStack) sig m
                  => ClassType -> m ()
beginClassType functionType = State.modify @ClassTypeStack
                               . overStack
                               $ Stack.push functionType

endClassType :: Has (State ClassTypeStack) sig m
                => m ()
endClassType = State.modify @ClassTypeStack
                . overStack
                $ Stack.pop_
