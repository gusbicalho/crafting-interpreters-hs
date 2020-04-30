module HSLox.StaticAnalysis.CheckBadThis
  ( preCheckBadThis
  , postCheckBadThis
  ) where

import Control.Carrier.State.Church (State)
import Control.Effect.Writer
import Control.Monad
import Data.Set (Set)
import qualified HSLox.AST as AST
import HSLox.AST.AsAST
import HSLox.AST.Meta
import HSLox.StaticAnalysis.Error
import qualified HSLox.StaticAnalysis.ClassTypeStack as ClassType

preCheckBadThis :: AsIdentity f
                      => AsAST a g
                      => Has (State ClassType.ClassTypeStack) sig m
                      => Has (Writer (Set AnalysisError)) sig m
                      => f a -> m (f a)
preCheckBadThis fa = do
  case content fa of
    (toThis -> Just (AST.This tk)) -> do
      fnType <- ClassType.currentClassType
      when (fnType == ClassType.None) $
        tellAnalysisError tk "Cannot use 'this' outside of a class."
    _ -> pure ()
  pure fa

postCheckBadThis :: Applicative m => f a -> m (f a)
postCheckBadThis = pure
