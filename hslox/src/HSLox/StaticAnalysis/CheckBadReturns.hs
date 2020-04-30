module HSLox.StaticAnalysis.CheckBadReturns
  ( preCheckBadReturns
  , postCheckBadReturns
  ) where

import Control.Carrier.State.Church (State)
import Control.Effect.Writer
import Control.Monad
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified HSLox.AST as AST
import HSLox.AST.AsAST
import HSLox.AST.Meta
import HSLox.StaticAnalysis.Error
import qualified HSLox.StaticAnalysis.FunctionTypeStack as FunctionType

preCheckBadReturns :: AsIdentity f
                      => AsAST a g
                      => Has (State FunctionType.FunctionTypeStack) sig m
                      => Has (Writer (Set AnalysisError)) sig m
                      => f a -> m (f a)
preCheckBadReturns fa = do
  case content fa of
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
