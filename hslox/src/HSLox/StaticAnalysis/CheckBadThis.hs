{-# LANGUAGE BlockArguments #-}

module HSLox.StaticAnalysis.CheckBadThis (walk) where

import Control.Algebra (Has)
import Control.Effect.State (State)
import Control.Effect.Writer (Writer)
import Control.Monad (when)
import Data.Function ((&))
import Data.Set (Set)
import HSLox.AST qualified as AST
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.AST.VisitAST (Const (Const), visitOnly_)
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
walk = Walker preCheckBadThis pure

preCheckBadThis ::
  Has (State ClassType.ClassTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WalkAST.Walk meta meta m
preCheckBadThis fa = do
  AST.Meta.content fa
    & visitOnly_ \(Const (AST.This tk)) -> do
      fnType <- ClassType.currentClassType
      when (fnType == ClassType.None) $
        tellAnalysisError tk "Cannot use 'this' outside of a class."
  pure fa
