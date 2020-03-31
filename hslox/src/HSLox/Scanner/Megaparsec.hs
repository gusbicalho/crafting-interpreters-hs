module HSLox.Scanner.Megaparsec where

import Control.Carrier.State.Church
import Control.Monad.Trans (lift)
import Data.Char (isLetter, isDigit)
import Data.Foldable (asum, for_)
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.Megaparsec hiding (State, Token)
import qualified Text.Megaparsec.Char as P.Char
import qualified Text.Megaparsec.Char.Lexer as P.L
import HSLox.TreeWalk.Error (Error (..))
import qualified HSLox.TreeWalk.Error as Error
import HSLox.TreeWalk.Token (Token (..))
import qualified HSLox.TreeWalk.Token as Token
import HSLox.TreeWalk.TokenType (TokenType)
import qualified HSLox.TreeWalk.TokenType as TokenType

scanTokens ::
  forall sig m. Has (State [Error]) sig m
             => T.Text
             -> m (Seq Token)
scanTokens source = do
    tks <- runParserT (manyTokensUntilEOF recover) "" source
    case tks of
      Left errorBundle -> do
        registerErrorBundle errorBundle
        pure (Seq.empty)
      Right tks' -> pure tks'
  where
    recover err = do
      lift $ register err
      pure Nothing
    registerErrorBundle errorBundle = for_ (bundleErrors errorBundle) register
    register (FancyError _ errs) = do
      for_ errs $ \case
        ErrorCustom e -> Error.reportError e
        err -> Error.reportError $ Error 0 "" (T.pack $ show err)
    register err =
      Error.reportError $ Error 0 "" (T.pack $ show err)

manyTokensUntilEOF :: MonadParsec Error T.Text f
                   => (ParseError T.Text Error -> f (Maybe Token))
                   -> f (Seq Token)
manyTokensUntilEOF recover = do
  space
  (tks, eof) <- manyTill_
                  (lexeme $ withRecovery recover (Just <$> nextToken))
                  (eof >> makeToken TokenType.EOF Nothing "")
  pure $ (Seq.fromList . catMaybes $ tks) Seq.|> eof

space :: MonadParsec Error T.Text m => m ()
space = P.L.space P.Char.space1 (P.L.skipLineComment "//") empty

lexeme :: MonadParsec Error T.Text m => m a -> m a
lexeme = P.L.lexeme space

symbol :: MonadParsec Error T.Text m => T.Text -> m T.Text
symbol = P.L.symbol space

nextToken :: MonadParsec Error T.Text m => m Token
nextToken =
    asum [ singleCharToken '(' TokenType.LEFT_PAREN
         , singleCharToken ')' TokenType.RIGHT_PAREN
         , singleCharToken '{' TokenType.LEFT_BRACE
         , singleCharToken '}' TokenType.RIGHT_BRACE
         , singleCharToken ',' TokenType.COMMA
         , singleCharToken '.' TokenType.DOT
         , singleCharToken '-' TokenType.MINUS
         , singleCharToken '+' TokenType.PLUS
         , singleCharToken ';' TokenType.SEMICOLON
         , singleCharToken '*' TokenType.STAR
         , singleCharToken '/' TokenType.SLASH -- line comments // are taken care of by `space`
         , oneTwoCharToken '!' TokenType.BANG
                           '=' TokenType.BANG_EQUAL
         , oneTwoCharToken '=' TokenType.EQUAL
                           '=' TokenType.EQUAL_EQUAL
         , oneTwoCharToken '<' TokenType.LESS
                           '=' TokenType.LESS_EQUAL
         , oneTwoCharToken '>' TokenType.GREATER
                           '=' TokenType.GREATER_EQUAL
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

makeError :: MonadParsec Error T.Text m
            => T.Text -> m (Set.Set (ErrorFancy Error))
makeError msg = do
  line <- unPos . sourceLine <$> getSourcePos
  pure . Set.singleton . ErrorCustom $ Error line "" msg

makeToken :: MonadParsec Error T.Text m => TokenType -> Maybe Token.LiteralValue -> T.Text -> m Token
makeToken tkType tkLiteral lexeme = do
  line <- unPos . sourceLine <$> getSourcePos
  pure Token { tokenType = tkType
             , tokenLiteral = tkLiteral
             , tokenLexeme = lexeme
             , tokenLine = line
             }

singleCharToken :: MonadParsec Error T.Text m => Char -> TokenType -> m Token
singleCharToken c tkType =
  try (makeToken tkType Nothing =<< symbol (T.singleton c))

oneTwoCharToken :: MonadParsec Error T.Text f => Char -> TokenType -> Char -> TokenType -> f Token
oneTwoCharToken c1 oneCharType c2 twoCharsType =
    asum [ try . lexeme $ P.Char.char c1 <* notFollowedBy (P.Char.char c2)
                            *> makeToken oneCharType Nothing oneCharSymbol
          , try . lexeme $ P.Char.char c1 *> P.Char.char c2
                            *> makeToken twoCharsType Nothing twoCharSymbol
          ]
  where
    oneCharSymbol = T.singleton c1
    twoCharSymbol = oneCharSymbol `T.snoc` c2

stringToken :: MonadParsec Error T.Text m => m Token
stringToken = do
  openQuote <- try (P.Char.char '\"')
  s <- T.pack <$> many (anySingleBut '\"')
  closeQuote <- P.Char.char '\"' <|> (fancyFailure =<< makeError "Unterminated string.")
  makeToken TokenType.STRING
            (Just $ Token.LitString s)
            (openQuote `T.cons` s `T.snoc` closeQuote)

numberToken :: MonadParsec Error T.Text m => m Token
numberToken = try $ do
  wholeDigits <- some P.Char.digitChar
  decimals <- try (do dot <- P.Char.char '.'
                      fracDigits <- some P.Char.digitChar
                      pure $ dot : fracDigits)
              <|> pure ""
  let numberText = wholeDigits <> decimals
  makeToken TokenType.NUMBER
            (Just . Token.LitNum . read @Double $ numberText)
            (T.pack numberText)

isIdentifierFirst :: Char -> Bool
isIdentifierFirst c = c == '_' || isLetter c

isIdentifierPart :: Char -> Bool
isIdentifierPart c = isIdentifierFirst c || isDigit c

identifierOrKeywordToken :: MonadParsec Error T.Text m => m Token
identifierOrKeywordToken = try $ do
  init <- takeWhile1P (Just "") isIdentifierFirst
  rest <- takeWhileP (Just "") isIdentifierPart
  let identifier = init <> rest
  case Map.lookup identifier lexemeToKeywordType of
    Nothing -> makeToken TokenType.IDENTIFIER Nothing identifier
    Just tkType -> makeToken tkType Nothing identifier

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
