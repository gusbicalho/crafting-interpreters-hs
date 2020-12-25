{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.Scanner.ScanError where

import Control.Effect.Writer
import qualified Data.Set as Set
import qualified Data.Text as T
import HSLox.ErrorReport

data ScanError
  = ScanError
  { scanErrorLine :: Int
  , scanErrorWhere :: T.Text
  , scanErrorMessage :: T.Text
  }
  deriving stock (Eq, Show, Ord)

instance ToErrorReport ScanError where
  toErrorReport ScanError { scanErrorLine, scanErrorWhere, scanErrorMessage }
    = ErrorReport { errorReportLine = scanErrorLine
                  , errorReportWhere = scanErrorWhere
                  , errorReportMessage = scanErrorMessage
                  }

reportScanError :: _ => ScanError -> m ()
reportScanError error = tell (Set.singleton error)
