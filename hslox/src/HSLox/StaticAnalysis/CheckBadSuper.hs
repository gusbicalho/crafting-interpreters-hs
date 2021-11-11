module HSLox.StaticAnalysis.CheckBadSuper (walk) where

import Control.Algebra (Has)
import Control.Effect.State (State)
import Control.Effect.Writer (Writer)
import Data.Set (Set)
import HSLox.AST qualified as AST
import HSLox.AST.AsAST (AsAST (..))
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.AST.WalkAST (Walker (Walker))
import HSLox.AST.WalkAST qualified as WalkAST
import HSLox.StaticAnalysis.ClassTypeStack qualified as ClassType
import HSLox.StaticAnalysis.Error (
  AnalysisError,
  tellAnalysisError,
 )

walk ::
  Has (State ClassType.ClassTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WalkAST.NeutralWalker input output m
walk = Walker preCheckBadSuper pure

preCheckBadSuper ::
  Has (State ClassType.ClassTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WalkAST.Walk meta meta m
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
