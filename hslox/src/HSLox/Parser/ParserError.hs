{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.Parser.ParserError where

import Control.Effect.Writer
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import HSLox.ErrorReport
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token

data ParserError
  = ParserError
  { parserErrorToken :: Token
  , parserErrorMessage :: T.Text
  }
  deriving (Eq, Show, Ord)

instance ToErrorReport ParserError where
  toErrorReport ParserError { parserErrorToken, parserErrorMessage }
    = ErrorReport { errorReportLine = (tokenLine parserErrorToken)
                  , errorReportWhere = case (tokenType parserErrorToken) of
                      Token.EOF -> " at end"
                      _ -> " at '" <> (tokenLexeme parserErrorToken) <> "'"
                  , errorReportMessage = parserErrorMessage
                  }

reportError :: _ => ParserError -> m ()
reportError error = tell (Seq.singleton error)
