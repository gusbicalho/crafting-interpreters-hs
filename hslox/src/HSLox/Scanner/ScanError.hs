{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.Scanner.ScanError where

import Control.Effect.Writer
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import HSLox.ErrorReport

data ScanError
  = ScanError
  { scanErrorLine :: Int
  , scanErrorWhere :: T.Text
  , scanErrorMessage :: T.Text
  }
  deriving (Eq, Show, Ord)

instance ToErrorReport ScanError where
  toErrorReport ScanError { scanErrorLine, scanErrorWhere, scanErrorMessage }
    = ErrorReport { errorReportLine = scanErrorLine
                  , errorReportWhere = scanErrorWhere
                  , errorReportMessage = scanErrorMessage
                  }

reportScanError :: _ => ScanError -> m ()
reportScanError error = tell (Seq.singleton error)
