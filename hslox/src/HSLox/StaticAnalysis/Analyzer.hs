module HSLox.StaticAnalysis.Analyzer (
  module HSLox.StaticAnalysis.Analyzer,
  AnalysisError,
  ResolveLocals.ResolverMeta (..),
) where

import Control.Algebra (Has)
import Control.Carrier.State.Church qualified as State
import Control.Effect.Writer (Writer)
import Data.Set (Set)
import HSLox.AST qualified as AST
import HSLox.AST.WalkAST (WalkAST (walkAST), (/>/))
import HSLox.StaticAnalysis.CheckBadReturns qualified as CheckBadReturns
import HSLox.StaticAnalysis.CheckBadSuper qualified as CheckBadSuper
import HSLox.StaticAnalysis.CheckBadSuperclass qualified as CheckBadSuperclass
import HSLox.StaticAnalysis.CheckBadThis qualified as CheckBadThis
import HSLox.StaticAnalysis.ClassTypeStack qualified as ClassTypeStack
import HSLox.StaticAnalysis.Error (AnalysisError)
import HSLox.StaticAnalysis.FunctionTypeStack qualified as FunctionTypeStack
import HSLox.StaticAnalysis.ResolveLocals qualified as ResolveLocals

analyze ::
  Has (Writer (Set AnalysisError)) sig m =>
  AST.Program meta ->
  m (AST.Program (ResolveLocals.ResolverMeta, meta))
analyze =
  State.evalState FunctionTypeStack.emptyState
    . State.evalState ClassTypeStack.emptyState
    . State.evalState ResolveLocals.emptyState
    . walkAST
      ( FunctionTypeStack.walk
          />/ ClassTypeStack.walk
          />/ ResolveLocals.walk
          />/ CheckBadReturns.walk
          />/ CheckBadThis.walk
          />/ CheckBadSuperclass.walk
          />/ CheckBadSuper.walk
      )
