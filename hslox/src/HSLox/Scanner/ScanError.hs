{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module HSLox.Scanner.ScanError where

import Control.Effect.Writer qualified as Writer
import Data.Set qualified as Set
import Data.Text qualified as T
import HSLox.ErrorReport (ErrorReport (..), ToErrorReport (..))

data ScanError = ScanError
  { scanErrorLine :: Int
  , scanErrorWhere :: T.Text
  , scanErrorMessage :: T.Text
  }
  deriving stock (Eq, Show, Ord)

instance ToErrorReport ScanError where
  toErrorReport ScanError{scanErrorLine, scanErrorWhere, scanErrorMessage} =
    ErrorReport
      { errorReportLine = scanErrorLine
      , errorReportWhere = scanErrorWhere
      , errorReportMessage = scanErrorMessage
      }

reportScanError :: _ => ScanError -> m ()
reportScanError error = Writer.tell (Set.singleton error)
