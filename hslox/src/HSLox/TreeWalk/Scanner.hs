module HSLox.TreeWalk.Scanner where

import Control.Carrier.Empty.Church
import Control.Carrier.State.Church
import Control.Carrier.Writer.Church
import Control.Monad (forever)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified HSLox.TreeWalk.TokenType as TokenType
import HSLox.TreeWalk.Token (Token (..))
import qualified HSLox.TreeWalk.Token as Token

scanTokens :: T.Text -> Seq Token
scanTokens source
    = run
    . evalState (initialScanState source)
    . execWriter @(Seq Token)
    $ do
      runEmptyToMaybe . forever $ do
        resetStart
        maybeToken <- runEmptyToMaybe scanNextToken
        case maybeToken of
          Nothing -> empty
          Just token -> addToken token
      endline <- gets scanStateLine
      addToken $ Token "" TokenType.EOF Nothing endline
  where
    addToken token = tell $ Seq.singleton token

data ScanState
  = ScanState
  { scanStateSource :: T.Text
  , scanStateStart :: Int
  , scanStateCurrent :: Int
  , scanStateLine :: Int
  }

initialScanState :: T.Text -> ScanState
initialScanState source
  = ScanState { scanStateSource = source
              , scanStateStart = 0
              , scanStateCurrent = 0
              , scanStateLine = 1
              }

runEmptyToMaybe :: Applicative m => EmptyC m a -> m (Maybe a)
runEmptyToMaybe = runEmpty (pure Nothing) (pure . Just)

resetStart :: Has (State ScanState) sig m
           => m ()
resetStart = modify $ \s@(ScanState { scanStateCurrent }) -> s { scanStateStart = scanStateCurrent }

incLine :: Has (State ScanState) sig m
        => m ()
incLine = modify $ \s@(ScanState { scanStateLine }) -> s { scanStateLine = succ scanStateLine }

incCurrent :: Has (State ScanState) sig m
        => m ()
incCurrent = modify $ \s@(ScanState { scanStateCurrent }) -> s { scanStateCurrent = succ scanStateCurrent }

nextChar :: Has Empty sig m
         => Has (State ScanState) sig m
         => m Char
nextChar = do
  state <- get
  case T.uncons (scanStateSource state) of
    Nothing -> empty
    Just (c, source') -> do
      incCurrent
      put state { scanStateSource = source' }
      pure c

scanNextToken :: Has Empty sig m
              => Has (State ScanState) sig m
              => m Token
scanNextToken = do
  c <- nextChar
  if c == '\n'
  then do
    incLine
    scanNextToken
  else do
    line <- gets scanStateLine
    pure $ Token (T.singleton c) TokenType.DOT (Just $ Token.LitBool True) line
