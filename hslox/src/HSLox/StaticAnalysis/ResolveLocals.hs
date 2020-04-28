module HSLox.StaticAnalysis.ResolveLocals where

import qualified Control.Carrier.Fresh.Church as Fresh
import Control.Effect.Writer
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import HSLox.AST
import HSLox.AST.AnnotateAST
import HSLox.AST.Meta
import HSLox.ErrorReport

newtype ResolverError = ResolverError T.Text
  deriving (Eq, Ord, Show)

tellResolverError :: Has (Writer (Set ResolverError)) sig m => T.Text -> m ()
tellResolverError = tell . Set.singleton . ResolverError

instance ToErrorReport ResolverError where
  toErrorReport (ResolverError msg) =
    ErrorReport { errorReportLine = 0
                , errorReportWhere = ""
                , errorReportMessage = msg
                }

newtype ResolverMeta = ResolverMeta Int
  deriving (Eq, Ord, Show)

resolveLocals :: AsIdentity f
              => Traversable f
              => Has (Writer (Set ResolverError)) sig m
              => Program f
              -> m (Program (WithMeta ResolverMeta f))
resolveLocals (Program stmts) = Program <$> Fresh.evalFresh 0 (traverse (annotateAST annotate) stmts)
  where
    annotate :: Has Fresh.Fresh sig m
             => f a -> m (WithMeta ResolverMeta f a)
    annotate fa = do
      id <- Fresh.fresh
      pure $ withMeta (ResolverMeta id) fa
