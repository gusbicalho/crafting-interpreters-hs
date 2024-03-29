module HSLox.Scanner.ByTheBook.ScanState where

import Control.Algebra (Has)
import Control.Effect.Empty (Empty)
import Control.Effect.Empty qualified as Empty
import Control.Effect.State (State)
import Control.Effect.State qualified as State
import Data.Text qualified as T

data ScanState = ScanState
  { scanStateSource :: T.Text
  , scanStateSegment :: T.Text
  , scanStateStart :: Int
  , scanStateCurrent :: Int
  , scanStateLine :: Int
  }

initialScanState :: T.Text -> ScanState
initialScanState source =
  ScanState
    { scanStateSource = source
    , scanStateSegment = mempty
    , scanStateStart = 0
    , scanStateCurrent = 0
    , scanStateLine = 1
    }

resetSegment ::
  Has (State ScanState) sig m =>
  m ()
resetSegment = State.modify $ \s@ScanState{scanStateCurrent} ->
  s
    { scanStateStart = scanStateCurrent
    , scanStateSegment = mempty
    }

incLine ::
  Has (State ScanState) sig m =>
  m ()
incLine = State.modify $ \s@ScanState{scanStateLine} -> s{scanStateLine = succ scanStateLine}

incCurrent ::
  Has (State ScanState) sig m =>
  m ()
incCurrent = State.modify $ \s@ScanState{scanStateCurrent} -> s{scanStateCurrent = succ scanStateCurrent}

isAtEnd ::
  Has (State ScanState) sig m =>
  m Bool
isAtEnd = do
  sourceRemaining <- State.gets scanStateSource
  pure $ sourceRemaining /= mempty

getLine ::
  Has (State ScanState) sig m =>
  m Int
getLine = State.gets scanStateLine

getSource ::
  Has (State ScanState) sig m =>
  m T.Text
getSource = State.gets scanStateSource

getSegment ::
  Has (State ScanState) sig m =>
  m T.Text
getSegment = State.gets scanStateSegment

advance ::
  Has Empty sig m =>
  Has (State ScanState) sig m =>
  m Char
advance = do
  state <- State.get
  case T.uncons (scanStateSource state) of
    Nothing -> Empty.empty
    Just (c, source') -> do
      incCurrent
      State.put
        state
          { scanStateSource = source'
          , scanStateSegment = T.snoc (scanStateSegment state) c
          }
      pure c

peek ::
  Has Empty sig m =>
  Has (State ScanState) sig m =>
  m Char
peek = do
  state <- State.get
  case T.uncons (scanStateSource state) of
    Nothing -> Empty.empty
    Just (c, _) -> pure c

peek2 ::
  Has Empty sig m =>
  Has (State ScanState) sig m =>
  m (Char, Char)
peek2 = do
  state <- State.get
  case T.uncons (scanStateSource state) of
    Nothing -> Empty.empty
    Just (c, source') ->
      case T.uncons source' of
        Nothing -> Empty.empty
        Just (c2, _) -> pure (c, c2)

match ::
  Has (State ScanState) sig m =>
  Char ->
  m Bool
match expected = do
  state <- State.get
  case T.uncons (scanStateSource state) of
    Nothing -> pure False
    Just (c, source')
      | expected /= c -> pure False
      | otherwise -> do
        incCurrent
        State.put
          state
            { scanStateSource = source'
            , scanStateSegment = T.snoc (scanStateSegment state) c
            }
        pure True
