module HSLox.StaticAnalysis.CheckBadReturns (
  preCheckBadReturns,
  postCheckBadReturns,
) where

import Control.Algebra (Has)
import Control.Effect.State (State)
import Control.Effect.Writer (Writer)
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Set (Set)
import HSLox.AST qualified as AST
import HSLox.AST.AsAST (AsAST (..))
import HSLox.AST.Meta (AsIdentity)
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.StaticAnalysis.Error (
  AnalysisError,
  tellAnalysisError,
 )
import HSLox.StaticAnalysis.FunctionTypeStack qualified as FunctionType

preCheckBadReturns ::
  AsIdentity f =>
  AsAST a g =>
  Has (State FunctionType.FunctionTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  f a ->
  m (f a)
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

postCheckBadReturns :: Applicative m => f a -> m (f a)
postCheckBadReturns = pure
