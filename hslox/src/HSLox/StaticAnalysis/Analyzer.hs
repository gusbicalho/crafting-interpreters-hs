module HSLox.StaticAnalysis.Analyzer (
  module HSLox.StaticAnalysis.Analyzer,
  AnalysisError,
  ResolveLocals.ResolverMeta (..),
) where

import Control.Algebra (Has)
import Control.Carrier.State.Church qualified as State
import Control.Effect.Writer (Writer)
import Control.Monad ((>=>))
import Data.Set (Set)
import HSLox.AST qualified as AST
import HSLox.AST.Meta (AsIdentity, WithMeta)
import HSLox.AST.WalkAST (WalkAST (walkAST))
import HSLox.StaticAnalysis.CheckBadReturns qualified as CheckBadReturns
import HSLox.StaticAnalysis.CheckBadSuper qualified as CheckBadSuper
import HSLox.StaticAnalysis.CheckBadSuperclass qualified as CheckBadSuperclass
import HSLox.StaticAnalysis.CheckBadThis qualified as CheckBadThis
import HSLox.StaticAnalysis.ClassTypeStack qualified as ClassTypeStack
import HSLox.StaticAnalysis.Error (AnalysisError)
import HSLox.StaticAnalysis.FunctionTypeStack qualified as FunctionTypeStack
import HSLox.StaticAnalysis.ResolveLocals qualified as ResolveLocals

analyze ::
  AsIdentity f =>
  Traversable f =>
  Has (Writer (Set AnalysisError)) sig m =>
  AST.Program f ->
  m (AST.Program (WithMeta ResolveLocals.ResolverMeta f))
analyze =
  State.evalState FunctionTypeStack.emptyState
    . State.evalState ClassTypeStack.emptyState
    . State.evalState ResolveLocals.emptyState
    . walkAST
      ( FunctionTypeStack.preFunctionTypeStack
          >=> ClassTypeStack.preClassTypeStack
          >=> ResolveLocals.preResolvingLocals
          >=> CheckBadReturns.preCheckBadReturns
          >=> CheckBadThis.preCheckBadThis
          >=> CheckBadSuperclass.preCheckBadSuperclass
          >=> CheckBadSuper.preCheckBadSuper
      )
      ( CheckBadSuper.postCheckBadSuper
          >=> CheckBadSuperclass.postCheckBadSuperclass
          >=> CheckBadThis.postCheckBadThis
          >=> CheckBadReturns.postCheckBadReturns
          >=> ResolveLocals.postResolvingLocals
          >=> ClassTypeStack.postClassTypeStack
          >=> FunctionTypeStack.postFunctionTypeStack
      )
