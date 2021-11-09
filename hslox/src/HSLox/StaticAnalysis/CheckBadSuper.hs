module HSLox.StaticAnalysis.CheckBadSuper (
  preCheckBadSuper,
  postCheckBadSuper,
) where

import Control.Algebra (Has)
import Control.Effect.State (State)
import Control.Effect.Writer (Writer)
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

preCheckBadSuper ::
  AsAST a g =>
  Has (State ClassType.ClassTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WithMeta meta a ->
  m (WithMeta meta a)
preCheckBadSuper fa = do
  case AST.Meta.content fa of
    (toSuper -> Just (AST.Super tk _)) -> do
      fnType <- ClassType.currentClassType
      case fnType of
        ClassType.None -> tellAnalysisError tk "Cannot use 'super' outside of a class."
        ClassType.Class -> tellAnalysisError tk "Cannot use 'super' in a class with no superclass."
        _ -> pure ()
    _ -> pure ()
  pure fa

postCheckBadSuper :: Applicative m => a -> m a
postCheckBadSuper = pure
