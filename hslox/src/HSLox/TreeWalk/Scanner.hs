module HSLox.TreeWalk.Scanner where

import Control.Carrier.Empty.Church
import Control.Carrier.State.Church
import Control.Carrier.Writer.Church
import Control.Monad (forever)
import Data.Bool (bool)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import HSLox.TreeWalk.TokenType (TokenType)
import qualified HSLox.TreeWalk.TokenType as TokenType
import HSLox.TreeWalk.Token (Token (..))
import qualified HSLox.TreeWalk.Token as Token
import HSLox.TreeWalk.Error (Error (..))
import qualified HSLox.TreeWalk.Error as Error

scanTokens :: Has (State [Error]) sig m
           => T.Text -> m (Seq Token)
scanTokens source
    = evalState (initialScanState source)
    . execWriter @(Seq Token)
    $ do
      runEmptyToMaybe . forever $ do
        resetSegment
        maybeToken <- runEmptyToMaybe scanNextToken
        case maybeToken of
          Nothing -> pure ()
          Just token -> addToken token
        guard =<< isAtEnd
      endline <- gets scanStateLine
      addToken $ Token "" TokenType.EOF Nothing endline
  where
    addToken token = tell $ Seq.singleton token

testScan :: T.Text -> ([Error], Seq Token)
testScan = run
         . runState @[Error] (\s a -> pure (s,a)) []
         . scanTokens

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

runEmptyToMaybe :: Applicative m => EmptyC m a -> m (Maybe a)
runEmptyToMaybe = runEmpty (pure Nothing) (pure . Just)

runEmptyToUnit :: Applicative m => EmptyC m a -> m ()
runEmptyToUnit = runEmpty (pure ()) (const $ pure ())

resetSegment :: Has (State ScanState) sig m
             => m ()
resetSegment = modify $ \s@(ScanState { scanStateCurrent }) ->
  s { scanStateStart = scanStateCurrent
    , scanStateSegment = T.empty
    }

incLine :: Has (State ScanState) sig m
        => m ()
incLine = modify $ \s@(ScanState { scanStateLine }) -> s { scanStateLine = succ scanStateLine }

incCurrent :: Has (State ScanState) sig m
        => m ()
incCurrent = modify $ \s@(ScanState { scanStateCurrent }) -> s { scanStateCurrent = succ scanStateCurrent }

isAtEnd :: Has (State ScanState) sig m
        => m Bool
isAtEnd = do
  sourceRemaining <- gets scanStateSource
  pure $ sourceRemaining /= T.empty

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

makeToken :: Has (State ScanState) sig m
          => TokenType -> Maybe Token.LiteralValue -> m Token
makeToken tkType tkLiteral = do
  ScanState { scanStateSegment, scanStateLine } <- get
  pure Token { tokenType = tkType
             , tokenLiteral = tkLiteral
             , tokenLexeme = scanStateSegment
             , tokenLine = scanStateLine
             }

reportError :: Has (State ScanState) sig m
            => Has (State [Error]) sig m
            => T.Text -> m ()
reportError msg = do
  line <- gets scanStateLine
  Error.reportError $ Error line "" msg

scanNextToken :: Has Empty sig m
              => Has (State [Error]) sig m
              => Has (State ScanState) sig m
              => m Token
scanNextToken = do
  c <- advance
  case c of
    '(' -> makeToken TokenType.LEFT_PAREN  Nothing
    ')' -> makeToken TokenType.RIGHT_PAREN Nothing
    '{' -> makeToken TokenType.LEFT_BRACE  Nothing
    '}' -> makeToken TokenType.RIGHT_BRACE Nothing
    ',' -> makeToken TokenType.COMMA       Nothing
    '.' -> makeToken TokenType.DOT         Nothing
    '-' -> makeToken TokenType.MINUS       Nothing
    '+' -> makeToken TokenType.PLUS        Nothing
    ';' -> makeToken TokenType.SEMICOLON   Nothing
    '*' -> makeToken TokenType.STAR        Nothing
    '!' -> match '=' >>= bool (makeToken TokenType.BANG          Nothing)
                              (makeToken TokenType.BANG_EQUAL    Nothing)
    '=' -> match '=' >>= bool (makeToken TokenType.EQUAL         Nothing)
                              (makeToken TokenType.EQUAL_EQUAL   Nothing)
    '<' -> match '=' >>= bool (makeToken TokenType.LESS          Nothing)
                              (makeToken TokenType.LESS_EQUAL    Nothing)
    '>' -> match '=' >>= bool (makeToken TokenType.GREATER       Nothing)
                              (makeToken TokenType.GREATER_EQUAL Nothing)
    '/' -> match '/' >>= bool (makeToken TokenType.SLASH Nothing)
                              (lineComment >> empty)
    ' ' -> empty
    '\r' -> empty
    '\t' -> empty
    '\n' -> incLine >> empty
    '"' -> makeToken TokenType.STRING . Just . Token.LitString =<< stringLit
    _ -> do
      reportError $ "Unexpected character: " `T.snoc` c
      empty

lineComment :: Has (State ScanState) sig m
            => m ()
lineComment
  = runEmpty (pure ()) (const $ pure ())
  . forever
  $ do c <- peek
       guard $ c /= '\n'
       advance

stringLit :: Has Empty sig m
          => Has (State [Error]) sig m
          => Has (State ScanState) sig m
          => m T.Text
stringLit = do
    runEmptyToUnit . forever $ do
      c <- peek
      guard $ c /= '"'
      advance
    runEmpty unterminatedStringError pure $ do
      advance
      seg <- gets scanStateSegment
      pure . T.drop 1 . T.dropEnd 1 $ seg
  where
    unterminatedStringError = do
      reportError $ "Unterminated string."
      empty
