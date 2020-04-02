module HSLox.Parser.ByTheBook.ParserState where

import Control.Effect.Empty
import Control.Effect.State
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import HSLox.Token (Token (..), TokenType)
import qualified HSLox.Util as Util

data ParserState
  = ParserState { parserStateTokens :: Seq Token
                , parserStatePrevious :: Maybe Token
                }

initialParserState :: Seq Token -> ParserState
initialParserState source
  = ParserState { parserStateTokens = source
                , parserStatePrevious = Nothing
                }

match :: Has Empty sig m
      => Has (State ParserState) sig m
      => Foldable t
      => t TokenType -> m Token
match tkTypes = do
  Util.backingUpState @ParserState $ \restore -> do
    tk <- advance
    if (any ((tokenType tk) ==) tkTypes)
    then
      pure tk
    else do
      restore
      empty

peek :: Has Empty sig m
     => Has (State ParserState) sig m
     => m Token
peek = fst <$> unconsTokens

advance :: Has Empty sig m
        => Has (State ParserState) sig m
        => m Token
advance = do
  (tk, tks) <- unconsTokens
  put ParserState { parserStateTokens = tks
                  , parserStatePrevious = Just tk
                  }
  pure tk

unconsTokens :: Has Empty sig m
             => Has (State ParserState) sig m
             => m (Token, Seq Token)
unconsTokens = do
  state <- get
  case Seq.viewl (parserStateTokens state) of
    Seq.EmptyL -> empty
    tk Seq.:< tks -> pure (tk, tks)

currentLine :: Has (State ParserState) sig m
            => m Int
currentLine =
  (tokenLine <$> peek) `Util.recoverFromEmptyWith` do
    previous <- gets parserStatePrevious
    case previous of
      Nothing -> pure 0
      Just tk -> pure $ tokenLine tk
