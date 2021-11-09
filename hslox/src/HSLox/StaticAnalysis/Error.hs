module HSLox.StaticAnalysis.Error where

import Control.Algebra (Has)
import Control.Effect.Writer (Writer)
import Control.Effect.Writer qualified as Writer
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import HSLox.ErrorReport (ErrorReport (..), ToErrorReport (..))
import HSLox.Token (Token (..))

data AnalysisError = AnalysisError Token T.Text
  deriving stock (Eq, Ord, Show)

instance ToErrorReport AnalysisError where
  toErrorReport (AnalysisError token msg) =
    ErrorReport
      { errorReportLine = tokenLine token
      , errorReportWhere = tokenLexeme token
      , errorReportMessage = msg
      }

tellAnalysisError :: Has (Writer (Set AnalysisError)) sig m => Token -> T.Text -> m ()
tellAnalysisError tk msg = Writer.tell . Set.singleton $ AnalysisError tk msg
