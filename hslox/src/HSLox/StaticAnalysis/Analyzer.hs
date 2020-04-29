{-# LANGUAGE StrictData #-}
module HSLox.StaticAnalysis.Analyzer
  ( module HSLox.StaticAnalysis.Analyzer
  , AnalysisError
  , ResolveLocals.ResolverMeta (..)
  ) where

import qualified Control.Carrier.State.Church as State
import Control.Effect.Writer
import Control.Monad
import Data.Set (Set)
import qualified HSLox.AST as AST
import HSLox.AST.WalkAST
import HSLox.AST.Meta
import HSLox.StaticAnalysis.Error
import qualified HSLox.StaticAnalysis.CheckBadReturns as CheckBadReturns
import qualified HSLox.StaticAnalysis.ResolveLocals as ResolveLocals

analyze :: AsIdentity f
        => Traversable f
        => Has (Writer (Set AnalysisError)) sig m
        => AST.Program f
        -> m (AST.Program (WithMeta ResolveLocals.ResolverMeta f))
analyze prog
  = State.evalState ResolveLocals.emptyState
  . State.evalState CheckBadReturns.emptyState
  . walkAST (ResolveLocals.preResolvingLocals >=>
             CheckBadReturns.preCheckForBadReturns)
            (CheckBadReturns.postCheckForBadReturns >=>
             ResolveLocals.postResolvingLocals)
  $ prog
