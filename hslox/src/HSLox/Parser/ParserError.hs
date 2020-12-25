{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.Parser.ParserError where

import Control.Effect.Writer
import qualified Data.Set as Set
import qualified Data.Text as T
import HSLox.ErrorReport
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token

data ParserError
  = ParserError { parserErrorToken :: Maybe Token
                , parserErrorMessage :: T.Text
                }
  deriving stock (Eq, Show, Ord)

instance ToErrorReport ParserError where
  toErrorReport ParserError { parserErrorToken, parserErrorMessage }
    = case parserErrorToken of
        Nothing ->
          ErrorReport { errorReportLine = 0
                      , errorReportWhere = ""
                      , errorReportMessage = parserErrorMessage
                      }
        Just token ->
          ErrorReport { errorReportLine = tokenLine token
                      , errorReportWhere = case tokenType token of
                          Token.EOF -> " at end"
                          _ -> " at '" <> tokenLexeme token <> "'"
                      , errorReportMessage = parserErrorMessage
                      }

reportError :: _ => ParserError -> m ()
reportError error = tell (Set.singleton error)
