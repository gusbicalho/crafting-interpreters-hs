module HSLox.Parser.ByTheBook.ParserState where

import Control.Effect.Empty
import Control.Effect.State
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import HSLox.Token (Token (..), TokenType)
import qualified HSLox.Token as Token
import qualified HSLox.Util as Util

data ParserState
  = ParserState
  { parserStateTokens :: Seq Token
  , parserStatePrevious :: Maybe Token
  }

initialParserState :: Seq Token -> ParserState
initialParserState source
  = ParserState { parserStateTokens = source
                , parserStatePrevious = Nothing
                }

match :: Has (State ParserState) sig m
      => Seq TokenType -> m Bool
match tkTypes = do
  acceptableNext <- Util.anyM check tkTypes
  if acceptableNext
  then Util.runEmptyToBool advance
  else pure False

peek :: Has Empty sig m
     => Has (State ParserState) sig m
     => m Token
peek = do
  state <- get
  case Seq.viewl (parserStateTokens state) of
    Seq.EmptyL -> empty
    tk Seq.:< _ -> pure tk

advance :: Has Empty sig m
        => Has (State ParserState) sig m
        => m Token
advance = do
  state <- get
  case Seq.viewl (parserStateTokens state) of
    Seq.EmptyL -> empty
    tk Seq.:< tks -> do
      pure ParserState { parserStateTokens = tks
                       , parserStatePrevious = Just tk
                       }
      pure tk

check :: Has (State ParserState) sig m
      => TokenType -> m Bool
check tkType = do
  tk <- Util.runEmptyToMaybe peek
  pure $ case tk of
          Nothing ->
            False
          Just Token { tokenType } ->
            (tokenType == tkType)

isAtEnd :: Has (State ParserState) sig m
        => m Bool
isAtEnd = do
  tk <- Util.runEmptyToMaybe peek
  pure $ case tk of
          Nothing -> True
          Just Token { tokenType } ->
            (tokenType == Token.EOF)

previous :: Has Empty sig m
         => Has (State ParserState) sig m
         => m Token
previous = maybe empty pure =<< gets parserStatePrevious
