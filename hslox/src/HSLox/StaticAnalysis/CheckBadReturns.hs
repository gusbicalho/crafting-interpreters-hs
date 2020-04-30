module HSLox.StaticAnalysis.CheckBadReturns
  ( preCheckForBadReturns
  , postCheckForBadReturns
  ) where

import Control.Carrier.State.Church (State)
import Control.Effect.Writer
import Control.Monad
import Data.Set (Set)
import qualified HSLox.AST as AST
import HSLox.AST.AsAST
import HSLox.AST.Meta
import HSLox.StaticAnalysis.Error
import qualified HSLox.StaticAnalysis.FunctionTypeStack as FunctionType

preCheckForBadReturns :: AsIdentity f
                      => AsAST a g
                      => Has (State FunctionType.FunctionTypeStack) sig m
                      => Has (Writer (Set AnalysisError)) sig m
                      => f a -> m (f a)
preCheckForBadReturns fa = do
  case content fa of
    (toReturn -> Just (AST.Return tk _)) -> do
      fnType <- FunctionType.currentFunctionType
      when (fnType == FunctionType.None) $
        tellAnalysisError tk "Cannot return from top-level code."
    _ -> pure ()
  pure fa

postCheckForBadReturns :: Applicative m => f a -> m (f a)
postCheckForBadReturns = pure
