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
import qualified HSLox.StaticAnalysis.ClassTypeStack as ClassTypeStack
import qualified HSLox.StaticAnalysis.FunctionTypeStack as FunctionTypeStack
import qualified HSLox.StaticAnalysis.CheckBadReturns as CheckBadReturns
import qualified HSLox.StaticAnalysis.CheckBadThis as CheckBadThis
import qualified HSLox.StaticAnalysis.ResolveLocals as ResolveLocals

analyze :: AsIdentity f
        => Traversable f
        => Has (Writer (Set AnalysisError)) sig m
        => AST.Program f
        -> m (AST.Program (WithMeta ResolveLocals.ResolverMeta f))
analyze
  = State.evalState FunctionTypeStack.emptyState
  . State.evalState ClassTypeStack.emptyState
  . State.evalState ResolveLocals.emptyState
  . walkAST (FunctionTypeStack.preFunctionTypeStack >=>
             ClassTypeStack.preClassTypeStack >=>
             ResolveLocals.preResolvingLocals >=>
             CheckBadReturns.preCheckBadReturns >=>
             CheckBadThis.preCheckBadThis)
            (CheckBadThis.postCheckBadThis >=>
             CheckBadReturns.postCheckBadReturns >=>
             ResolveLocals.postResolvingLocals >=>
             ClassTypeStack.postClassTypeStack >=>
             FunctionTypeStack.postFunctionTypeStack)
