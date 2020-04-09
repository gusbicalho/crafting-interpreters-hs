module HSLox.Parser.ByTheBook.Parser where

import qualified Control.Effect.Error as ErrorEff
import Control.Effect.Empty
import Control.Carrier.Empty.Church (EmptyC)
import Control.Carrier.State.Church
import Control.Carrier.Writer.Church
import Control.Monad (void)
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import HSLox.AST
import HSLox.Parser.ParserError
import HSLox.Parser.ByTheBook.ParserState
import HSLox.Token (Token (..), TokenType)
import qualified HSLox.Token as Token
import qualified HSLox.Util as Util

parse :: Has (Writer (Seq ParserError)) sig m
      => (Seq Token)
      -> m Program
parse tokens
  = evalState (initialParserState tokens)
  . fmap Program
  . execWriter @(Seq Stmt)
  . Util.untilEmpty
  $ do
    stmt' <- Util.runErrorToEither @ParserError statement
    case stmt' of
      Left error -> do
        reportError error
        synchronize
      Right stmt -> do
        tell $ Seq.singleton stmt
    guard . not =<< isAtEnd

type ExprParser sig m = ( Has (ErrorEff.Error ParserError) sig m
                        , Has (State ParserState) sig m )
                        => m Expr

type StmtParser sig m = ( Has (ErrorEff.Error ParserError) sig m
                        , Has (State ParserState) sig m )
                        => m Stmt

synchronize :: Has (State ParserState) sig m => m ()
synchronize = Util.untilEmpty $ do
  tk <- advance
  guard $ (tokenType tk) /= Token.SEMICOLON
  tk <- peek
  guard $ all ((tokenType tk) /=) [ Token.CLASS
                                  , Token.FUN
                                  , Token.VAR
                                  , Token.FOR
                                  , Token.IF
                                  , Token.WHILE
                                  , Token.PRINT
                                  , Token.RETURN
                                  ]

statement :: StmtParser sig m
statement = do
    stmt <- printStmt `Util.recoverFromEmptyWith` expressionStmt
    match [Token.SEMICOLON]
          `Util.recoverFromEmptyWith`
          throwParserError "Expect ';' after expression."
    pure stmt

printStmt :: Has Empty sig m => StmtParser sig m
printStmt = do
  tk <- match [ Token.PRINT ]
  expr <- expression
  pure . PrintStmt $ Print tk expr

expressionStmt :: StmtParser sig m
expressionStmt = ExprStmt <$> expression

expression :: ExprParser sig m
expression = comma

comma :: ExprParser sig m
comma = do
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression conditional opTypes ]
    leftAssociativeBinaryOp conditional opTypes
  where
    opTypes = [ Token.COMMA ]

conditional :: ExprParser sig m
conditional = do
  left <- equality
  mbTk <- Util.runEmptyToMaybe $ match [ Token.QUESTION_MARK ]
  case mbTk of
    Nothing -> pure left
    Just op1 -> do
      middle <- expression
      op2 <- match [ Token.COLON ]
              `Util.recoverFromEmptyWith`
              throwParserError "Expect ':' after '?' expression."
      right <- conditional
      pure $ TernaryE  left op1 middle op2 right

equality :: ExprParser sig m
equality = do
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression comparison opTypes ]
    leftAssociativeBinaryOp comparison opTypes
  where
    opTypes = [ Token.EQUAL_EQUAL
              , Token.BANG_EQUAL
              ]

comparison :: ExprParser sig m
comparison = do
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression addition opTypes ]
    leftAssociativeBinaryOp addition opTypes
  where
    opTypes = [ Token.GREATER
              , Token.GREATER_EQUAL
              , Token.LESS
              , Token.LESS_EQUAL
              ]

addition :: ExprParser sig m
addition = do
  checkForKnownErrorProductions
    [ binaryOperatorAtBeginningOfExpression multiplication [ Token.PLUS ] ]
  leftAssociativeBinaryOp multiplication [ Token.MINUS
                                         , Token.PLUS
                                         ]

multiplication :: ExprParser sig m
multiplication = do
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression unary opTypes ]
    leftAssociativeBinaryOp unary opTypes
  where
    opTypes =  [ Token.STAR
               , Token.SLASH
               ]

unary :: ExprParser sig m
unary = do
  mbTk <- Util.runEmptyToMaybe (match [Token.MINUS, Token.BANG])
  case mbTk of
    Nothing -> primary
    Just tk -> UnaryE tk <$> unary

primary :: ExprParser sig m
primary = do
    mbTk <- Util.runEmptyToMaybe (match [ Token.FALSE
                                        , Token.TRUE
                                        , Token.NIL
                                        , Token.NUMBER
                                        , Token.STRING
                                        , Token.LEFT_PAREN
                                        ])
    case mbTk of
      Just (Token { tokenType = Token.FALSE }) -> pure (BoolE False)
      Just (Token { tokenType = Token.TRUE })  -> pure (BoolE True)
      Just (Token { tokenType = Token.NIL })   -> pure NilE
      Just (Token { tokenType = Token.STRING, tokenLiteral }) ->
        case tokenLiteral of
          Just (Token.LitString s) -> pure (StringE s)
          _ -> throwParserError "SCANNER ERROR: Expected string literal in STRING token."
      Just (Token { tokenType = Token.NUMBER, tokenLiteral }) ->
        case tokenLiteral of
          Just (Token.LitNum n) -> pure (NumE n)
          _ -> throwParserError "SCANNER ERROR: Expected numeric literal in NUMBER token."
      Just (Token { tokenType = Token.LEFT_PAREN }) -> do
        expr <- expression
        match [Token.RIGHT_PAREN]
          `Util.recoverFromEmptyWith`
          throwParserError "Expect ')' after expression."
        pure $ GroupingE expr
      _ -> throwParserError "Expect expression."

makeParserError :: Has (ErrorEff.Throw ParserError) sig m
                => Has (State ParserState) sig m
                => T.Text -> m ParserError
makeParserError msg = do
    tk <- maybe eof id <$> Util.runEmptyToMaybe peek
    pure $ ParserError (Just tk) msg
  where
    eof = Token "" Token.EOF Nothing 0

throwParserError :: Has (ErrorEff.Throw ParserError) sig m
                 => Has (State ParserState) sig m
                => T.Text -> m b
throwParserError msg = ErrorEff.throwError =<< makeParserError msg

leftAssociativeBinaryOp :: Foldable t
                        => (forall sig m. ExprParser sig m)
                        -> t TokenType
                        -> ExprParser sig m
leftAssociativeBinaryOp termParser opTypes = do
  left <- termParser
  e <- execState left . Util.untilEmpty $ do
    left <- get
    op <- match opTypes
    right <- termParser
    put $ BinaryE left op right
  pure e

checkForKnownErrorProductions
  :: Foldable t
  => Has (State ParserState) sig m
  => t (EmptyC m a) -> m ()
checkForKnownErrorProductions errorProductions
  = Util.backingUpState @ParserState $ \restore -> do
      for_ errorProductions Util.runEmptyToUnit
      restore

binaryOperatorAtBeginningOfExpression
  :: Foldable t
  => Has (ErrorEff.Error ParserError) sig m
  => Has (State ParserState) sig m
  => Has Empty sig m
  => m a -> t TokenType -> m b
binaryOperatorAtBeginningOfExpression termParser opTypes = do
  op <- match opTypes
  let error = ParserError (Just op) $ "Binary operator "
                                   <> (tokenLexeme op)
                                   <> " found at the beginning of expression."
  synchronizeByConsuming termParser
  ErrorEff.throwError error

synchronizeByConsuming
  :: Has (ErrorEff.Catch ParserError) sig m
  => m a -> m ()
synchronizeByConsuming termParser =
  ErrorEff.catchError @ParserError
    (void termParser)
    (const $ pure ())
