module HSLox.ErrorReport where

import Data.Text qualified as T

data ErrorReport = ErrorReport
  { errorReportLine :: Int
  , errorReportWhere :: T.Text
  , errorReportMessage :: T.Text
  }
  deriving stock (Eq, Show, Ord)

class ToErrorReport e where
  toErrorReport :: e -> ErrorReport
