{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module HSLox.Parser.ParserError where

import Control.Effect.Writer qualified as Writer
import Data.Set qualified as Set
import Data.Text qualified as T
import HSLox.ErrorReport (ErrorReport (..), ToErrorReport (..))
import HSLox.Token (Token (..))
import HSLox.Token qualified as Token

data ParserError = ParserError
  { parserErrorToken :: Maybe Token
  , parserErrorMessage :: T.Text
  }
  deriving stock (Eq, Show, Ord)

instance ToErrorReport ParserError where
  toErrorReport ParserError{parserErrorToken, parserErrorMessage} =
    case parserErrorToken of
      Nothing ->
        ErrorReport
          { errorReportLine = 0
          , errorReportWhere = ""
          , errorReportMessage = parserErrorMessage
          }
      Just token ->
        ErrorReport
          { errorReportLine = tokenLine token
          , errorReportWhere = case tokenType token of
              Token.EOF -> " at end"
              _ -> " at '" <> tokenLexeme token <> "'"
          , errorReportMessage = parserErrorMessage
          }

reportError :: _ => ParserError -> m ()
reportError error = Writer.tell (Set.singleton error)
