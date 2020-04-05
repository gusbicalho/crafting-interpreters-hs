module HSLox.Parser.Megaparsec where

import Control.Effect.Error as ErrorEff
import Control.Carrier.State.Church
import Control.Carrier.Writer.Church
import Control.Monad.Trans (lift)
import Data.Foldable
import Data.Functor
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import HSLox.AST
import HSLox.Parser.ParserError (ParserError (..))
import qualified HSLox.Parser.ParserError as ParserError
import HSLox.Token (Token (..), TokenType)
import qualified HSLox.Token as Token
import HSLox.Parser.Megaparsec.TokenStream (TokenStream (..))
import Text.Megaparsec hiding (State, Token)

parse :: Has (Writer (Seq ParserError)) sig m
      => (Seq Token)
      -> m (Seq Expr)
parse tokens = do
    exprs <- runParserT nextExpr "" (TokenStream tokens)
    case exprs of
      Left errorBundle -> do
        registerErrorBundle errorBundle
        pure (Seq.empty)
      Right exprs' -> pure . Seq.singleton $ exprs'
  where
    recover err = do
      lift $ register err
      pure Nothing
    registerErrorBundle errorBundle = for_ (bundleErrors errorBundle) register
    register (FancyError _ errs) = do
      for_ errs $ \case
        ErrorCustom e -> ParserError.reportError e
        err -> ParserError.reportError $ ParserError Nothing (T.pack $ show err)
    register err =
      ParserError.reportError $ ParserError Nothing (T.pack $ show err)


makeError :: Maybe Token -> T.Text -> Set.Set (ErrorFancy ParserError)
makeError mbTk msg = Set.singleton . ErrorCustom $ ParserError mbTk msg

nextExpr :: MonadParsec ParserError TokenStream m => m Expr
nextExpr = expression

expression :: MonadParsec ParserError TokenStream m => m Expr
expression = comma

comma :: MonadParsec ParserError TokenStream m => m Expr
comma = leftAssociativeBinaryOp conditional [ Token.COMMA ]

conditional :: MonadParsec ParserError TokenStream m => m Expr
conditional = do
    left <- equality
    conditionalBody left <|> pure left
  where
    conditionalBody left = do
      op1 <- singleMatching [ Token.QUESTION_MARK ]
      middle <- expression
      op2 <- consume [ Token.COLON ] "Expect ':' after '?' expression."
      right <- conditional
      pure $ TernaryE left op1 middle op2 right

equality :: MonadParsec ParserError TokenStream m => m Expr
equality =
    leftAssociativeBinaryOp comparison opTypes
  where
    opTypes = [ Token.EQUAL_EQUAL
              , Token.BANG_EQUAL
              ]

comparison :: MonadParsec ParserError TokenStream m => m Expr
comparison =
    leftAssociativeBinaryOp addition opTypes
  where
    opTypes = [ Token.GREATER
              , Token.GREATER_EQUAL
              , Token.LESS
              , Token.LESS_EQUAL
              ]

addition :: MonadParsec ParserError TokenStream m => m Expr
addition =
    leftAssociativeBinaryOp multiplication opTypes
  where
    opTypes = [ Token.MINUS
              , Token.PLUS
              ]

multiplication :: MonadParsec ParserError TokenStream m => m Expr
multiplication =
    leftAssociativeBinaryOp unary opTypes
  where
    opTypes = [ Token.STAR
              , Token.SLASH
              ]

unary :: MonadParsec ParserError TokenStream m => m Expr
unary = (UnaryE <$> singleMatching [Token.MINUS, Token.BANG]
                <*> unary)
    <|> primary

primary :: MonadParsec ParserError TokenStream m => m Expr
primary =
  asum [ singleMatching [ Token.FALSE ] $> BoolE False
       , singleMatching [ Token.TRUE ]  $> BoolE True
       , singleMatching [ Token.NIL ]   $> NilE
       , do tk <- singleMatching [ Token.STRING ]
            case tokenLiteral tk of
              Just (Token.LitString s) -> pure (StringE s)
              _ -> fancyFailure $ makeError (Just tk) "SCANNER ERROR: Expected string literal in STRING token."
       , do tk <- singleMatching [ Token.NUMBER ]
            case tokenLiteral tk of
              Just (Token.LitNum n) -> pure (NumE n)
              _ -> fancyFailure $ makeError (Just tk) "SCANNER ERROR: Expected number literal in NUMBER token."
       , do singleMatching [ Token.LEFT_PAREN ]
            expr <- expression
            consume [ Token.RIGHT_PAREN ] "Expect ')' after expression."
            pure expr
       , do tk <- lookAhead maybeAny
            fancyFailure (makeError tk "Expect expression.")
       ]

leftAssociativeBinaryOp :: Foldable t
                        => MonadParsec ParserError TokenStream m
                        => m Expr -> t TokenType -> m Expr
leftAssociativeBinaryOp termParser opTypes = do
  left <- termParser
  following <- many ((,) <$> singleMatching opTypes
                         <*> termParser)
  pure $ foldl' (\l (op, r) -> BinaryE l op r) left following

singleMatching :: Foldable t
               => MonadParsec ParserError TokenStream m
               => t TokenType -> m Token
singleMatching tkTypes = try $ satisfy (\tk -> any ((tokenType tk) ==) tkTypes)

maybeAny :: MonadParsec ParserError TokenStream m => m (Maybe Token)
maybeAny = (Just <$> anySingle) <|> pure Nothing

consume :: Foldable t
        => MonadParsec ParserError TokenStream f
        => t TokenType -> T.Text -> f Token
consume tkTypes errorMsg
    = singleMatching tkTypes
  <|> do tk <- maybeAny
         fancyFailure (makeError tk errorMsg)
