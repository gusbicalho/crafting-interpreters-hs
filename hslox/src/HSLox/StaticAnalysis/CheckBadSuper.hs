module HSLox.StaticAnalysis.CheckBadSuper
  ( preCheckBadSuper
  , postCheckBadSuper
  ) where

import Control.Carrier.State.Church (State)
import Control.Effect.Writer
import Data.Set (Set)
import qualified HSLox.AST as AST
import HSLox.AST.AsAST
import HSLox.AST.Meta
import HSLox.StaticAnalysis.Error
import qualified HSLox.StaticAnalysis.ClassTypeStack as ClassType

preCheckBadSuper :: AsIdentity f
                 => AsAST a g
                 => Has (State ClassType.ClassTypeStack) sig m
                 => Has (Writer (Set AnalysisError)) sig m
                 => f a -> m (f a)
preCheckBadSuper fa = do
  case content fa of
    (toSuper -> Just (AST.Super tk _)) -> do
      fnType <- ClassType.currentClassType
      case fnType of
        ClassType.None -> tellAnalysisError tk "Cannot use 'super' outside of a class."
        ClassType.Class -> tellAnalysisError tk "Cannot use 'super' in a class with no superclass."
        _ -> pure ()
    _ -> pure ()
  pure fa

postCheckBadSuper :: Applicative m => f a -> m (f a)
postCheckBadSuper = pure
