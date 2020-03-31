module HSLox.Scanner.Parsec.Scanner where

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
    tks <- runParserT
             (manyTill_ (lexeme $ withRecovery recover (Just <$> nextToken))
                        (eof >> makeToken TokenType.EOF Nothing "")) "" source
    case tks of
      Left errors -> do
        for_ (bundleErrors errors) register
        pure (Seq.empty)
      Right (tks', eofTk) -> pure
                           . (Seq.|> eofTk)
                           . Seq.fromList
                           . catMaybes
                           $ tks'
  where
    recover err = do
      lift $ register err
      pure Nothing
    register (FancyError _ errs) = do
      for_ errs $ \case
        ErrorCustom e -> Error.reportError e
        err -> Error.reportError $ Error 0 "" (T.pack $ show err)
    register err =
      Error.reportError $ Error 0 "" (T.pack $ show err)

space :: MonadParsec Error T.Text m => m ()
space = P.L.space P.Char.space1 (P.L.skipLineComment "//") empty

lexeme :: MonadParsec Error T.Text m => m a -> m a
lexeme = P.L.lexeme space

symbol :: MonadParsec Error T.Text m => T.Text -> m T.Text
symbol = P.L.symbol space

match :: MonadParsec Error T.Text f => T.Text -> f Bool
match chunk = try (symbol chunk $> True) <|> pure False

nextToken :: MonadParsec Error T.Text m => m Token
nextToken = asum
          $ [ try (symbol "("  >>= makeToken TokenType.LEFT_PAREN    Nothing)
            , try (symbol ")"  >>= makeToken TokenType.RIGHT_PAREN   Nothing)
            , try (symbol "{"  >>= makeToken TokenType.LEFT_BRACE    Nothing)
            , try (symbol "}"  >>= makeToken TokenType.RIGHT_BRACE   Nothing)
            , try (symbol ","  >>= makeToken TokenType.COMMA         Nothing)
            , try (symbol "."  >>= makeToken TokenType.DOT           Nothing)
            , try (symbol "-"  >>= makeToken TokenType.MINUS         Nothing)
            , try (symbol "+"  >>= makeToken TokenType.PLUS          Nothing)
            , try (symbol ";"  >>= makeToken TokenType.SEMICOLON     Nothing)
            , try (symbol "*"  >>= makeToken TokenType.STAR          Nothing)
            , try (P.Char.char '!' <* notFollowedBy (P.Char.char '=')
                                <&> T.singleton
                                >>= makeToken TokenType.BANG          Nothing)
            , try (symbol "!=" >>= makeToken TokenType.BANG_EQUAL    Nothing)
            , try (symbol "<=" >>= makeToken TokenType.LESS_EQUAL    Nothing)
            , try (symbol "<"  >>= makeToken TokenType.LESS          Nothing)
            , try (symbol ">=" >>= makeToken TokenType.GREATER_EQUAL Nothing)
            , try (symbol ">"  >>= makeToken TokenType.GREATER       Nothing)
            , try (symbol "/"  >>= makeToken TokenType.SLASH         Nothing)
            , stringToken
            , numberToken
            , identifierOrKeywordToken
            , anySingle >>= makeError . ("Unexpected character: " `T.snoc`) >>= fancyFailure]

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
