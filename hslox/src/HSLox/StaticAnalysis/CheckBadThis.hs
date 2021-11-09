module HSLox.StaticAnalysis.CheckBadThis (
  preCheckBadThis,
  postCheckBadThis,
) where

import Control.Algebra (Has)
import Control.Effect.State (State)
import Control.Effect.Writer (Writer)
import Control.Monad (when)
import Data.Set (Set)
import HSLox.AST qualified as AST
import HSLox.AST.AsAST (AsAST (..))
import HSLox.AST.Meta (WithMeta)
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.StaticAnalysis.ClassTypeStack qualified as ClassType
import HSLox.StaticAnalysis.Error (
  AnalysisError,
  tellAnalysisError,
 )

preCheckBadThis ::
  AsAST a g =>
  Has (State ClassType.ClassTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WithMeta meta a ->
  m (WithMeta meta a)
preCheckBadThis fa = do
  case AST.Meta.content fa of
    (toThis -> Just (AST.This tk)) -> do
      fnType <- ClassType.currentClassType
      when (fnType == ClassType.None) $
        tellAnalysisError tk "Cannot use 'this' outside of a class."
    _ -> pure ()
  pure fa

postCheckBadThis :: Applicative m => a -> m a
postCheckBadThis = pure
