{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.Parser.ParserError where

import Control.Effect.Writer
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import HSLox.ErrorReport

data ParserError
  = ParserError
  { parserErrorLine :: Int
  , parserErrorWhere :: T.Text
  , parserErrorMessage :: T.Text
  }
  deriving (Eq, Show, Ord)

instance ToErrorReport ParserError where
  toErrorReport ParserError { parserErrorLine, parserErrorWhere, parserErrorMessage }
    = ErrorReport { errorReportLine = parserErrorLine
                  , errorReportWhere = parserErrorWhere
                  , errorReportMessage = parserErrorMessage
                  }

reportError :: _ => ParserError -> m ()
reportError error = tell (Seq.singleton error)
