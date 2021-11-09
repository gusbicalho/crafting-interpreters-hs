module HSLox.StaticAnalysis.CheckBadSuperclass (walk) where

import Control.Algebra (Has)
import Control.Effect.State (State)
import Control.Effect.Writer (Writer)
import Control.Monad (when)
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
import HSLox.Token (Token (..))

walk ::
  Has (State ClassType.ClassTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WalkAST.NeutralWalker input output m
walk = Walker preCheckBadSuperclass pure

preCheckBadSuperclass ::
  Has (State ClassType.ClassTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  WalkAST.PreWalk meta meta m
preCheckBadSuperclass fa = do
  case AST.Meta.content fa of
    (toClassDeclaration -> Just (AST.ClassDeclaration tk (Just superclass) _)) -> do
      let superclassTk = AST.variableIdentifier . AST.Meta.content $ superclass
      when (tokenLexeme tk == tokenLexeme superclassTk) $
        tellAnalysisError tk "A class cannot inherit from itself."
    _ -> pure ()
  pure fa
