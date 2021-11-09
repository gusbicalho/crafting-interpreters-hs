module HSLox.StaticAnalysis.CheckBadSuperclass (
  preCheckBadSuperclass,
  postCheckBadSuperclass,
) where

import Control.Algebra (Has)
import Control.Effect.State (State)
import Control.Effect.Writer (Writer)
import Control.Monad (when)
import Data.Set (Set)
import HSLox.AST qualified as AST
import HSLox.AST.AsAST (AsAST (..))
import HSLox.AST.Meta (AsIdentity)
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.StaticAnalysis.ClassTypeStack qualified as ClassType
import HSLox.StaticAnalysis.Error (
  AnalysisError,
  tellAnalysisError,
 )
import HSLox.Token (Token (..))

preCheckBadSuperclass ::
  AsIdentity f =>
  AsAST a f =>
  Has (State ClassType.ClassTypeStack) sig m =>
  Has (Writer (Set AnalysisError)) sig m =>
  f a ->
  m (f a)
preCheckBadSuperclass fa = do
  case AST.Meta.content fa of
    (toClassDeclaration -> Just (AST.ClassDeclaration tk (Just superclass) _)) -> do
      let superclassTk = AST.variableIdentifier . AST.Meta.content $ superclass
      when (tokenLexeme tk == tokenLexeme superclassTk) $
        tellAnalysisError tk "A class cannot inherit from itself."
    _ -> pure ()
  pure fa

postCheckBadSuperclass :: Applicative m => f a -> m (f a)
postCheckBadSuperclass = pure
