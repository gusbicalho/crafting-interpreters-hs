module HSLox.StaticAnalysis.CheckBadSuperclass
  ( preCheckBadSuperclass
  , postCheckBadSuperclass
  ) where

import Control.Carrier.State.Church (State)
import Control.Effect.Writer
import Control.Monad
import Data.Set (Set)
import qualified HSLox.AST as AST
import HSLox.AST.AsAST
import HSLox.AST.Meta
import HSLox.StaticAnalysis.Error
import qualified HSLox.StaticAnalysis.ClassTypeStack as ClassType
import HSLox.Token (Token (..))

preCheckBadSuperclass :: AsIdentity f
                      => AsAST a f
                      => Has (State ClassType.ClassTypeStack) sig m
                      => Has (Writer (Set AnalysisError)) sig m
                      => f a -> m (f a)
preCheckBadSuperclass fa = do
  case content fa of
    (toClassDeclaration -> Just (AST.ClassDeclaration tk (Just superclass) _)) -> do
      let superclassTk = AST.variableIdentifier . content $ superclass
      when (tokenLexeme tk == tokenLexeme superclassTk) $
        tellAnalysisError tk "A class cannot inherit from itself."
    _ -> pure ()
  pure fa

postCheckBadSuperclass :: Applicative m => f a -> m (f a)
postCheckBadSuperclass = pure
