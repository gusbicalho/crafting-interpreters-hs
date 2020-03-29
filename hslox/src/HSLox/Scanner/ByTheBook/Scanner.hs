module HSLox.Scanner.ByTheBook.Scanner where

import Prelude hiding (getLine)
import Control.Carrier.Empty.Church
import Control.Carrier.State.Church
import Control.Carrier.Writer.Church
import Data.Bool (bool)
import Data.Char (isDigit, isLetter)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import HSLox.Scanner.ByTheBook.ScanState
  ( ScanState
  , initialScanState
  , resetSegment
  , getSegment
  , getLine
  , incLine
  , advance
  , peek
  , peek2
  , match
  , isAtEnd
  )
import HSLox.TreeWalk.TokenType (TokenType)
import qualified HSLox.TreeWalk.TokenType as TokenType
import HSLox.TreeWalk.Token (Token (..))
import qualified HSLox.TreeWalk.Token as Token
import HSLox.TreeWalk.Error (Error (..))
import qualified HSLox.TreeWalk.Error as Error
import qualified HSLox.Util as Util

scanTokens ::
  forall sig m. Has (State [Error]) sig m
             => T.Text
             -> m (Seq Token)
scanTokens source
    = evalState (initialScanState source)
    . execWriter @(Seq Token)
    $ do
      Util.untilEmpty $ do
        resetSegment
        maybeToken <- scanNextToken
        Util.whenJust maybeToken addToken
        guard =<< isAtEnd
      addToken =<< buildEOFToken
  where
    addToken token = tell $ Seq.singleton token

makeToken :: Has (State ScanState) sig m
          => TokenType -> Maybe Token.LiteralValue -> m Token
makeToken tkType tkLiteral = do
  line <- getLine
  segment <- getSegment
  pure Token { tokenType = tkType
             , tokenLiteral = tkLiteral
             , tokenLexeme = segment
             , tokenLine = line
             }

reportError :: Has (State ScanState) sig m
            => Has (State [Error]) sig m
            => T.Text -> m ()
reportError msg = do
  line <- getLine
  Error.reportError $ Error line "" msg

buildEOFToken :: Has (State ScanState) sig m => m Token
buildEOFToken = do
  Token "" TokenType.EOF Nothing <$> getLine

scanNextToken :: Has Empty sig m
              => Has (State [Error]) sig m
              => Has (State ScanState) sig m
              => m (Maybe Token)
scanNextToken = Util.runEmptyToMaybe $ do
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
    _ | isDigit c -> makeToken TokenType.NUMBER . Just . Token.LitNum =<< numberLit
      | isIdentifierFirst c -> makeIdentifierToken
      | otherwise -> do
        reportError $ "Unexpected character: " `T.snoc` c
        empty

lineComment :: Has (State ScanState) sig m
            => m ()
lineComment = Util.untilEmpty $ do
  c <- peek
  guard $ c /= '\n'
  advance

stringLit :: Has Empty sig m
          => Has (State [Error]) sig m
          => Has (State ScanState) sig m
          => m T.Text
stringLit = do
    Util.untilEmpty $ do
      c <- peek
      guard $ c /= '"'
      advance
    runEmpty unterminatedStringError pure $ do
      advance
      seg <- getSegment
      pure . T.drop 1 . T.dropEnd 1 $ seg
  where
    unterminatedStringError = do
      reportError $ "Unterminated string."
      empty

numberLit :: Has (State ScanState) sig m
          => m Double
numberLit = do
    advanceWhileDigit
    Util.runEmptyToUnit $ do
      (dot, digit) <- peek2
      guard (dot == '.' && isDigit digit)
      advance
      advanceWhileDigit
    seg <- getSegment
    pure (read (T.unpack seg) :: Double)
  where
    advanceWhileDigit :: forall sig m. Has (State ScanState) sig m => m ()
    advanceWhileDigit = Util.untilEmpty $ do
      c <- peek
      guard $ isDigit c
      advance

isIdentifierFirst :: Char -> Bool
isIdentifierFirst c = c == '_' || isLetter c

isIdentifierPart :: Char -> Bool
isIdentifierPart c = isIdentifierFirst c || isDigit c

lexemeToKeywordType :: Map T.Text TokenType
lexemeToKeywordType =
  Map.fromList [ ("and",    TokenType.AND)
               , ("class",  TokenType.CLASS)
               , ("else",   TokenType.ELSE)
               , ("false",  TokenType.FALSE)
               , ("for",    TokenType.FOR)
               , ("fun",    TokenType.FUN)
               , ("if",     TokenType.IF)
               , ("nil",    TokenType.NIL)
               , ("or",     TokenType.OR)
               , ("print",  TokenType.PRINT)
               , ("return", TokenType.RETURN)
               , ("super",  TokenType.SUPER)
               , ("this",   TokenType.THIS)
               , ("true",   TokenType.TRUE)
               , ("var",    TokenType.VAR)
               , ("while",  TokenType.WHILE)
               ]

makeIdentifierToken :: Has (State ScanState) sig m
                    => m Token
makeIdentifierToken = do
  Util.untilEmpty $ do
    c <- peek
    guard $ isIdentifierPart c
    advance
  seg <- getSegment
  case Map.lookup seg lexemeToKeywordType of
    Nothing -> makeToken TokenType.IDENTIFIER Nothing
    Just tkType -> makeToken tkType Nothing
