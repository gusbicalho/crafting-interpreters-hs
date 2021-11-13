{-# LANGUAGE BlockArguments #-}

module HSLox.StaticAnalysis.ClassTypeStack (
  ClassTypeStack,
  ClassType (..),
  walk,
  emptyState,
  currentClassType,
) where

import Control.Algebra (Has)
import Control.Carrier.State.Church (State)
import Control.Carrier.State.Church qualified as State
import Data.Function ((&))
import Data.Maybe (fromMaybe, isJust)
import HSLox.AST qualified as AST
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.AST.VisitAST (visitOnly_)
import HSLox.AST.WalkAST (Walker (Walker))
import HSLox.AST.WalkAST qualified as WalkAST
import HSLox.StaticAnalysis.Stack (Stack)
import HSLox.StaticAnalysis.Stack qualified as Stack

walk ::
  Has (State ClassTypeStack) sig m =>
  WalkAST.NeutralWalker input output m
walk = Walker preClassTypeStack postClassTypeStack

preClassTypeStack ::
  Has (State ClassTypeStack) sig m =>
  WalkAST.Walk meta meta m
preClassTypeStack fa = do
  AST.Meta.content fa
    & visitOnly_ \(AST.ClassDeclaration _ super _) -> do
      beginClassType $
        if isJust super
          then Subclass
          else Class
  pure fa

postClassTypeStack ::
  Has (State ClassTypeStack) sig m =>
  WalkAST.Walk meta meta m
postClassTypeStack fa = do
  AST.Meta.content fa
    & visitOnly_ (\AST.ClassDeclaration{} -> endClassType)
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
