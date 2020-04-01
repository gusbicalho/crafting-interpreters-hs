module HSLox.Scanner.ByTheBook.Scanner where

import Prelude hiding (getLine)
import Control.Carrier.Empty.Church
import Control.Carrier.State.Church
import Control.Carrier.Writer.Church
import Data.Bool (bool)
import Data.Char (isDigit, isLetter)
import Data.Foldable (for_)
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
import HSLox.Token (Token (..), TokenType)
import qualified HSLox.Token as Token
import HSLox.Error (Error (..))
import qualified HSLox.Error as Error
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
        for_ maybeToken addToken
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
  Token "" Token.EOF Nothing <$> getLine

scanNextToken :: Has Empty sig m
              => Has (State [Error]) sig m
              => Has (State ScanState) sig m
              => m (Maybe Token)
scanNextToken = Util.runEmptyToMaybe $ do
  c <- advance
  case c of
    '(' -> makeToken Token.LEFT_PAREN  Nothing
    ')' -> makeToken Token.RIGHT_PAREN Nothing
    '{' -> makeToken Token.LEFT_BRACE  Nothing
    '}' -> makeToken Token.RIGHT_BRACE Nothing
    ',' -> makeToken Token.COMMA       Nothing
    '.' -> makeToken Token.DOT         Nothing
    '-' -> makeToken Token.MINUS       Nothing
    '+' -> makeToken Token.PLUS        Nothing
    ';' -> makeToken Token.SEMICOLON   Nothing
    '*' -> makeToken Token.STAR        Nothing
    '!' -> match '=' >>= bool (makeToken Token.BANG          Nothing)
                              (makeToken Token.BANG_EQUAL    Nothing)
    '=' -> match '=' >>= bool (makeToken Token.EQUAL         Nothing)
                              (makeToken Token.EQUAL_EQUAL   Nothing)
    '<' -> match '=' >>= bool (makeToken Token.LESS          Nothing)
                              (makeToken Token.LESS_EQUAL    Nothing)
    '>' -> match '=' >>= bool (makeToken Token.GREATER       Nothing)
                              (makeToken Token.GREATER_EQUAL Nothing)
    '/' -> match '/' >>= bool (makeToken Token.SLASH Nothing)
                              (lineComment >> empty)
    ' ' -> empty
    '\r' -> empty
    '\t' -> empty
    '\n' -> incLine >> empty
    '"' -> makeToken Token.STRING . Just . Token.LitString =<< stringLit
    _ | isDigit c -> makeToken Token.NUMBER . Just . Token.LitNum =<< numberLit
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
  Map.fromList [ ("and",    Token.AND)
               , ("class",  Token.CLASS)
               , ("else",   Token.ELSE)
               , ("false",  Token.FALSE)
               , ("for",    Token.FOR)
               , ("fun",    Token.FUN)
               , ("if",     Token.IF)
               , ("nil",    Token.NIL)
               , ("or",     Token.OR)
               , ("print",  Token.PRINT)
               , ("return", Token.RETURN)
               , ("super",  Token.SUPER)
               , ("this",   Token.THIS)
               , ("true",   Token.TRUE)
               , ("var",    Token.VAR)
               , ("while",  Token.WHILE)
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
    Nothing -> makeToken Token.IDENTIFIER Nothing
    Just tkType -> makeToken tkType Nothing
