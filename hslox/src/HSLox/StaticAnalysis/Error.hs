module HSLox.StaticAnalysis.Error where

import Control.Effect.Writer
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import HSLox.ErrorReport
import HSLox.Token (Token (..))

data AnalysisError = AnalysisError Token T.Text
  deriving (Eq, Ord, Show)

instance ToErrorReport AnalysisError where
  toErrorReport (AnalysisError token msg) =
    ErrorReport { errorReportLine = (tokenLine token)
                , errorReportWhere = (tokenLexeme token)
                , errorReportMessage = msg
                }

tellAnalysisError :: Has (Writer (Set AnalysisError)) sig m => Token -> T.Text -> m ()
tellAnalysisError tk msg = tell . Set.singleton $ AnalysisError tk msg
