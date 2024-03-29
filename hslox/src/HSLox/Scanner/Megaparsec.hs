module HSLox.Scanner.Megaparsec where

import Control.Algebra (Has)
import Control.Effect.Writer (Writer)
import Control.Monad.Trans (lift)
import Data.Char (isDigit, isLetter)
import Data.Foldable (asum, for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import HSLox.Scanner.ScanError (ScanError (..))
import HSLox.Scanner.ScanError qualified as ScanError
import HSLox.Token (Token (..), TokenType)
import HSLox.Token qualified as Token
import Text.Megaparsec hiding (State, Token)
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.L

scanTokens ::
  forall sig m.
  Has (Writer (Set ScanError)) sig m =>
  T.Text ->
  m (Seq Token)
{-# INLINABLE scanTokens #-}
scanTokens source = do
  tks <- runParserT (manyTokensUntilEOF recover) "" source
  case tks of
    Left errorBundle -> do
      registerErrorBundle errorBundle
      pure Seq.empty
    Right tks' -> pure tks'
 where
  recover err = do
    lift $ register err
    pure Nothing
  registerErrorBundle errorBundle = for_ (bundleErrors errorBundle) register
  register (FancyError _ errs) = do
    for_ errs $ \case
      ErrorCustom e -> ScanError.reportScanError e
      err -> ScanError.reportScanError $ ScanError 0 "" (T.pack $ show err)
  register err =
    ScanError.reportScanError $ ScanError 0 "" (T.pack $ show err)

manyTokensUntilEOF ::
  MonadParsec ScanError T.Text f =>
  (ParseError T.Text ScanError -> f (Maybe Token)) ->
  f (Seq Token)
{-# INLINABLE manyTokensUntilEOF #-}
manyTokensUntilEOF recover = do
  space
  (tks, eof) <-
    manyTill_
      (lexeme $ withRecovery recover (Just <$> nextToken))
      (eof >> makeToken Token.EOF Nothing "")
  pure $ (Seq.fromList . catMaybes $ tks) Seq.|> eof

space :: MonadParsec ScanError T.Text m => m ()
space = P.L.space P.Char.space1 (P.L.skipLineComment "//") empty

lexeme :: MonadParsec ScanError T.Text m => m a -> m a
lexeme = P.L.lexeme space

symbol :: MonadParsec ScanError T.Text m => T.Text -> m T.Text
symbol = P.L.symbol space

nextToken :: MonadParsec ScanError T.Text m => m Token
nextToken =
  asum
    [ singleCharToken '(' Token.LEFT_PAREN
    , singleCharToken ')' Token.RIGHT_PAREN
    , singleCharToken '{' Token.LEFT_BRACE
    , singleCharToken '}' Token.RIGHT_BRACE
    , singleCharToken ',' Token.COMMA
    , singleCharToken ':' Token.COLON
    , singleCharToken '.' Token.DOT
    , singleCharToken '-' Token.MINUS
    , singleCharToken '+' Token.PLUS
    , singleCharToken '?' Token.QUESTION_MARK
    , singleCharToken ';' Token.SEMICOLON
    , singleCharToken '*' Token.STAR
    , singleCharToken '/' Token.SLASH -- line comments // are taken care of by `space`
    , oneTwoCharToken
        '!'
        Token.BANG
        '='
        Token.BANG_EQUAL
    , oneTwoCharToken
        '='
        Token.EQUAL
        '='
        Token.EQUAL_EQUAL
    , oneTwoCharToken
        '<'
        Token.LESS
        '='
        Token.LESS_EQUAL
    , oneTwoCharToken
        '>'
        Token.GREATER
        '='
        Token.GREATER_EQUAL
    , stringToken
    , numberToken
    , identifierOrKeywordToken
    , unexpectedChar
    ]
 where
  unexpectedChar = do
    c <- anySingle
    error <- makeError ("Unexpected character: " `T.snoc` c)
    fancyFailure error

makeError ::
  MonadParsec ScanError T.Text m =>
  T.Text ->
  m (Set.Set (ErrorFancy ScanError))
makeError msg = do
  line <- unPos . sourceLine <$> getSourcePos
  pure . Set.singleton . ErrorCustom $ ScanError line "" msg

makeToken :: MonadParsec ScanError T.Text m => TokenType -> Maybe Token.LiteralValue -> T.Text -> m Token
makeToken tkType tkLiteral lexeme = do
  line <- unPos . sourceLine <$> getSourcePos
  pure
    Token
      { tokenType = tkType
      , tokenLiteral = tkLiteral
      , tokenLexeme = lexeme
      , tokenLine = line
      }

singleCharToken :: MonadParsec ScanError T.Text m => Char -> TokenType -> m Token
singleCharToken c tkType =
  try (makeToken tkType Nothing . T.singleton =<< P.Char.char c)

oneTwoCharToken :: MonadParsec ScanError T.Text f => Char -> TokenType -> Char -> TokenType -> f Token
oneTwoCharToken c1 oneCharType c2 twoCharsType =
  asum
    [ try $
        P.Char.char c1 <* notFollowedBy (P.Char.char c2)
          *> makeToken oneCharType Nothing oneCharSymbol
    , try $
        P.Char.char c1 *> P.Char.char c2
          *> makeToken twoCharsType Nothing twoCharSymbol
    ]
 where
  oneCharSymbol = T.singleton c1
  twoCharSymbol = oneCharSymbol `T.snoc` c2

stringToken :: MonadParsec ScanError T.Text m => m Token
stringToken = do
  openQuote <- try (P.Char.char '\"')
  s <- T.pack <$> many (anySingleBut '\"')
  closeQuote <- P.Char.char '\"' <|> (fancyFailure =<< makeError "Unterminated string.")
  makeToken
    Token.STRING
    (Just $ Token.LitString s)
    (openQuote `T.cons` s `T.snoc` closeQuote)

numberToken :: MonadParsec ScanError T.Text m => m Token
numberToken = try $ do
  wholeDigits <- some P.Char.digitChar
  decimals <-
    try
      ( do
          dot <- P.Char.char '.'
          fracDigits <- some P.Char.digitChar
          pure $ dot : fracDigits
      )
      <|> pure ""
  let numberText = wholeDigits <> decimals
  makeToken
    Token.NUMBER
    (Just . Token.LitNum . read @Double $ numberText)
    (T.pack numberText)

isIdentifierFirst :: Char -> Bool
isIdentifierFirst c = c == '_' || isLetter c

isIdentifierPart :: Char -> Bool
isIdentifierPart c = isIdentifierFirst c || isDigit c

identifierOrKeywordToken :: MonadParsec ScanError T.Text m => m Token
identifierOrKeywordToken = try $ do
  init <- takeWhile1P (Just "") isIdentifierFirst
  rest <- takeWhileP (Just "") isIdentifierPart
  let identifier = init <> rest
  case Map.lookup identifier lexemeToKeywordType of
    Nothing -> makeToken Token.IDENTIFIER Nothing identifier
    Just tkType -> makeToken tkType Nothing identifier

lexemeToKeywordType :: Map T.Text TokenType
lexemeToKeywordType =
  Map.fromList
    [ ("and", Token.AND)
    , ("class", Token.CLASS)
    , ("else", Token.ELSE)
    , ("false", Token.FALSE)
    , ("for", Token.FOR)
    , ("fun", Token.FUN)
    , ("if", Token.IF)
    , ("nil", Token.NIL)
    , ("or", Token.OR)
    , ("return", Token.RETURN)
    , ("super", Token.SUPER)
    , ("this", Token.THIS)
    , ("true", Token.TRUE)
    , ("var", Token.VAR)
    , ("while", Token.WHILE)
    ]
