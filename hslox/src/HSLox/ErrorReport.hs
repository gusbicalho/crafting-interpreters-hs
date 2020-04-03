{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.ErrorReport where

import qualified Data.Text as T

data ErrorReport
  = ErrorReport
  { errorReportLine :: Int
  , errorReportWhere :: T.Text
  , errorReportMessage :: T.Text
  }
  deriving (Eq, Show, Ord)

class ToErrorReport e where
  toErrorReport :: e -> ErrorReport
