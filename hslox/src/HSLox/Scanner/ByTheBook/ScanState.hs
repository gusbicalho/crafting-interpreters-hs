module HSLox.Scanner.ByTheBook.ScanState where

import Control.Effect.Empty
import Control.Effect.State
import qualified Data.Text as T

data ScanState
  = ScanState
  { scanStateSource :: T.Text
  , scanStateSegment :: T.Text
  , scanStateStart :: Int
  , scanStateCurrent :: Int
  , scanStateLine :: Int
  }

initialScanState :: T.Text -> ScanState
initialScanState source
  = ScanState { scanStateSource = source
              , scanStateSegment = T.empty
              , scanStateStart = 0
              , scanStateCurrent = 0
              , scanStateLine = 1
              }

resetSegment :: Has (State ScanState) sig m
             => m ()
resetSegment = modify $ \s@ScanState { scanStateCurrent } ->
  s { scanStateStart = scanStateCurrent
    , scanStateSegment = T.empty
    }

incLine :: Has (State ScanState) sig m
        => m ()
incLine = modify $ \s@ScanState { scanStateLine } -> s { scanStateLine = succ scanStateLine }

incCurrent :: Has (State ScanState) sig m
        => m ()
incCurrent = modify $ \s@ScanState { scanStateCurrent } -> s { scanStateCurrent = succ scanStateCurrent }

isAtEnd :: Has (State ScanState) sig m
        => m Bool
isAtEnd = do
  sourceRemaining <- gets scanStateSource
  pure $ sourceRemaining /= T.empty

getLine :: Has (State ScanState) sig m
          => m Int
getLine = gets scanStateLine

getSource :: Has (State ScanState) sig m
          => m T.Text
getSource = gets scanStateSource

getSegment :: Has (State ScanState) sig m
           => m T.Text
getSegment = gets scanStateSegment

advance :: Has Empty sig m
        => Has (State ScanState) sig m
        => m Char
advance = do
  state <- get
  case T.uncons (scanStateSource state) of
    Nothing -> empty
    Just (c, source') -> do
      incCurrent
      put state { scanStateSource = source'
                , scanStateSegment = T.snoc (scanStateSegment state) c
                }
      pure c

peek :: Has Empty sig m
     => Has (State ScanState) sig m
     => m Char
peek = do
  state <- get
  case T.uncons (scanStateSource state) of
    Nothing -> empty
    Just (c, _) -> pure c

peek2 :: Has Empty sig m
      => Has (State ScanState) sig m
      => m (Char, Char)
peek2 = do
  state <- get
  case T.uncons (scanStateSource state) of
    Nothing -> empty
    Just (c, source') ->
      case T.uncons source' of
        Nothing -> empty
        Just (c2, _) -> pure (c, c2)

match :: Has (State ScanState) sig m
      => Char -> m Bool
match expected = do
  state <- get
  case T.uncons (scanStateSource state) of
    Nothing -> pure False
    Just (c, source')
      | expected /= c -> pure False
      | otherwise -> do
        incCurrent
        put state { scanStateSource = source'
                  , scanStateSegment = T.snoc (scanStateSegment state) c
                  }
        pure True
