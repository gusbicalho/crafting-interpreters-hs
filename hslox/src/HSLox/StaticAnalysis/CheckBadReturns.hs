module HSLox.StaticAnalysis.CheckBadReturns (walk) where

import Control.Algebra (Has)
import Control.Effect.State (State)
import Control.Effect.Writer (Writer)
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Set (Set)
import HSLox.AST qualified as AST
import HSLox.AST.AsAST (AsAST (..))
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.AST.WalkAST qualified as WalkAST
import HSLox.StaticAnalysis.Error (
  AnalysisError,
  tellAnalysisError,
 )
import HSLox.StaticAnalysis.FunctionTypeStack qualified as FunctionType

walk ::
  Has (State FunctionType.FunctionTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WalkAST.NeutralWalker input output m
walk = WalkAST.Walker preCheckBadReturns pure

preCheckBadReturns ::
  Has (State FunctionType.FunctionTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WalkAST.Walk meta meta m
preCheckBadReturns fa = do
  case AST.Meta.content fa of
    (toReturn -> Just (AST.Return tk expr)) -> do
      fnType <- FunctionType.currentFunctionType
      when (fnType == FunctionType.None) $
        tellAnalysisError tk "Cannot return from top-level code."
      when (fnType == FunctionType.Initializer && isJust expr) $
        tellAnalysisError tk "Cannot return a value from an initializer."
    _ -> pure ()
  pure fa
