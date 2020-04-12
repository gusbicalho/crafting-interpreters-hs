module HSLox.Parser.Megaparsec where

import Control.Effect.Error as ErrorEff
import Control.Carrier.Writer.Church
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Data.Maybe (catMaybes)
import Data.Foldable
import Data.Functor
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import HSLox.AST
import HSLox.AST.Sugar
import HSLox.Parser.ParserError (ParserError (..))
import qualified HSLox.Parser.ParserError as ParserError
import HSLox.Token (Token (..), TokenType)
import qualified HSLox.Token as Token
import HSLox.Parser.Megaparsec.TokenStream (TokenStream (..))
import Text.Megaparsec hiding (State, Token)

parse :: Has (Writer (Seq ParserError)) sig m
      => (Seq Token)
      -> m Program
parse tokens = do
    stmts' <- runParserT (manyStmtsUntilEOF (lift . register)) "" (TokenStream tokens)
    case stmts' of
      Left errorBundle -> do
        for_ (bundleErrors errorBundle) register
        pure $ Program Seq.empty
      Right stmts -> pure $ Program stmts
  where
    register (FancyError _ errs) = do
      for_ errs $ \case
        ErrorCustom e -> ParserError.reportError e
        err -> ParserError.reportError $ ParserError Nothing (T.pack $ show err)
    register err =
      ParserError.reportError $ ParserError Nothing (T.pack $ show err)

manyStmtsUntilEOF :: MonadParsec ParserError TokenStream m
                  => (ParseError TokenStream ParserError -> m ())
                  -> m (Seq Stmt)
manyStmtsUntilEOF handleError =
    Seq.fromList . catMaybes <$>
      manyTill
        (withRecovery recover (Just <$> declaration))
        (eof <|> void (singleMatching [ Token.EOF ]))
  where
    recover err = do
      handleError err
      skipManyTill anySingle (synchronizationPoint <|> eof)
      pure Nothing
    synchronizationPoint = try $ do
      tk <- anySingle
      when (tokenType tk /= Token.SEMICOLON) $
        void . lookAhead . singleMatching $ [ Token.CLASS
                                            , Token.FUN
                                            , Token.VAR
                                            , Token.FOR
                                            , Token.IF
                                            , Token.WHILE
                                            , Token.PRINT
                                            , Token.RETURN
                                            ]

makeError :: Maybe Token -> T.Text -> Set.Set (ErrorFancy ParserError)
makeError mbTk msg = Set.singleton . ErrorCustom $ ParserError mbTk msg

declaration :: MonadParsec ParserError TokenStream m => m Stmt
declaration = do
  varDeclaration <|> statement

varDeclaration :: MonadParsec ParserError TokenStream m => m Stmt
varDeclaration = do
    singleMatching [Token.VAR]
    identifier <- consume [Token.IDENTIFIER] "Expect variable name."
    init <- asum [ singleMatching [Token.EQUAL] *> expression
                 , pure NilE
                 ]
    consume [Token.SEMICOLON] "Expect ';' after variable declaration."
    pure . DeclarationStmt $ VarDeclaration identifier init

statement :: MonadParsec ParserError TokenStream m => m Stmt
statement = asum [ printStmt
                 , blockStmt
                 , ifStmt
                 , whileStmt
                 , forStmt
                 , expressionStmt
                 ]

ifStmt :: MonadParsec ParserError TokenStream m => m Stmt
ifStmt = do
  singleMatching [ Token.IF ]
  consume [ Token.LEFT_PAREN ] "Expect '(' after 'if'."
  condition <- expression
  consume [ Token.RIGHT_PAREN ] "Expect ')' after if condition."
  thenStmt <- statement
  elseStmt <- do _ <- singleMatching [ Token.ELSE ]
                 Just <$> statement
              <|> pure Nothing
  pure . IfStmt $ If condition thenStmt elseStmt

whileStmt :: MonadParsec ParserError TokenStream m => m Stmt
whileStmt = do
  singleMatching [ Token.WHILE ]
  consume [ Token.LEFT_PAREN ] "Expect '(' after 'while'."
  condition <- expression
  consume [ Token.RIGHT_PAREN ] "Expect ')' after while condition."
  body <- statement
  pure . WhileStmt $ While condition body

forStmt :: MonadParsec ParserError TokenStream m => m Stmt
forStmt = do
    singleMatching [ Token.FOR ]
    consume [ Token.LEFT_PAREN ] "Expect '(' after 'for'."
    init      <- asum [ (singleMatching [ Token.SEMICOLON ] $> Nothing)
                      , (Just <$> varDeclaration)
                      , (Just <$> expressionStmt)
                      ]
    condition <- asum [ (singleMatching [ Token.SEMICOLON ] $> Nothing)
                      , (Just <$> expression
                          <* consume [ Token.SEMICOLON ] "Expect ';' after for condition.")
                      ]
    increment <- asum [ (singleMatching [ Token.RIGHT_PAREN ] $> Nothing)
                      , (Just <$> expression
                          <* consume [ Token.RIGHT_PAREN ] "Expect ')' after for increment.")
                      ]
    body <- statement
    pure $ buildFor init condition increment body

blockStmt :: MonadParsec ParserError TokenStream m => m Stmt
blockStmt = do
    singleMatching [ Token.LEFT_BRACE ]
    stmts <- blockBody Seq.empty
    consume [ Token.RIGHT_BRACE ] "Expect '}' after block."
    pure . BlockStmt . Block $ stmts
  where
    blockBody stmts =
      do (void . lookAhead $ singleMatching [ Token.RIGHT_BRACE, Token.EOF ]) <|> eof
         pure stmts
      <|> do stmt <- declaration
             blockBody (stmts Seq.:|> stmt)


printStmt :: MonadParsec ParserError TokenStream m => m Stmt
printStmt = do
  tk <- singleMatching [ Token.PRINT ]
  expr <- expression
  consume [ Token.SEMICOLON ] "Expect ';' after value."
  pure . PrintStmt $ Print tk expr

expressionStmt :: MonadParsec ParserError TokenStream m => m Stmt
expressionStmt = do
  expr <- expression
  consume [ Token.SEMICOLON ] "Expect ';' after expression."
  pure $ ExprStmt expr

expression :: MonadParsec ParserError TokenStream m => m Expr
expression = comma

comma :: MonadParsec ParserError TokenStream m => m Expr
comma =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression assignment opTypes ]
      <|> leftAssociativeBinaryOp BinaryE assignment opTypes
  where
    opTypes = [ Token.COMMA ]

assignment :: MonadParsec ParserError TokenStream m => m Expr
assignment = do
    left <- conditional
    assignTo left <|> pure left
  where
    assignTo left = do
      equals <- singleMatching [ Token.EQUAL ]
      right <- assignment
      case left of
        VariableE tk -> pure $ AssignmentE tk right
        _ -> do
          registerFancyFailure (makeError (Just equals) "Invalid assignment target.")
          empty

conditional :: MonadParsec ParserError TokenStream m => m Expr
conditional = do
    left <- orExpr
    conditionalBody left <|> pure left
  where
    conditionalBody left = do
      op1 <- singleMatching [ Token.QUESTION_MARK ]
      middle <- expression
      op2 <- consume [ Token.COLON ] "Expect ':' after '?' expression."
      right <- conditional
      pure $ TernaryE left op1 middle op2 right

orExpr :: MonadParsec ParserError TokenStream m => m Expr
orExpr =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression andExpr opTypes ]
      <|> leftAssociativeBinaryOp LogicalE andExpr opTypes
  where
    opTypes = [ Token.OR ]

andExpr :: MonadParsec ParserError TokenStream m => m Expr
andExpr =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression equality opTypes ]
      <|> leftAssociativeBinaryOp LogicalE equality opTypes
  where
    opTypes = [ Token.AND ]

equality :: MonadParsec ParserError TokenStream m => m Expr
equality =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression comparison opTypes ]
      <|> leftAssociativeBinaryOp BinaryE comparison opTypes
  where
    opTypes = [ Token.EQUAL_EQUAL
              , Token.BANG_EQUAL
              ]

comparison :: MonadParsec ParserError TokenStream m => m Expr
comparison =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression addition opTypes ]
      <|> leftAssociativeBinaryOp BinaryE addition opTypes
  where
    opTypes = [ Token.GREATER
              , Token.GREATER_EQUAL
              , Token.LESS
              , Token.LESS_EQUAL
              ]

addition :: MonadParsec ParserError TokenStream m => m Expr
addition =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression multiplication [ Token.PLUS ] ]
      <|> leftAssociativeBinaryOp BinaryE multiplication [ Token.MINUS
                                                         , Token.PLUS
                                                         ]

multiplication :: MonadParsec ParserError TokenStream m => m Expr
multiplication =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression unary opTypes ]
      <|> leftAssociativeBinaryOp BinaryE unary opTypes
  where
    opTypes = [ Token.STAR
              , Token.SLASH
              ]

unary :: MonadParsec ParserError TokenStream m => m Expr
unary = (UnaryE <$> singleMatching [Token.MINUS, Token.BANG]
                <*> unary)
    <|> call

call :: MonadParsec ParserError TokenStream m => m Expr
call = primary >>= sequenceOfCalls
  where
    sequenceOfCalls callee = do
      nextCallParen <- (True <$ singleMatching [ Token.LEFT_PAREN ]) <|> pure False
      if nextCallParen
      then do
        args <- arguments
        paren <- consume [ Token.RIGHT_PAREN ] "Expect ')' after arguments."
        sequenceOfCalls (CallE callee paren args)
      else pure callee
    arguments = do
      endOfArgsList <- lookAhead $ (True <$ singleMatching [ Token.LEFT_PAREN ])
                               <|> pure False
      if endOfArgsList
      then pure Seq.empty
      else argumentsList Seq.empty
    argumentsList args = do
      arg <- assignment
      followedByComma <- (True <$ singleMatching [ Token.COMMA ]) <|> pure False
      args <- pure $ args Seq.:|> arg
      if followedByComma
      then argumentsList args
      else pure args

primary :: MonadParsec ParserError TokenStream m => m Expr
primary =
  asum [ singleMatching [ Token.FALSE ]       $> BoolE False
       , singleMatching [ Token.TRUE ]        $> BoolE True
       , singleMatching [ Token.NIL ]         $> NilE
       , singleMatching [ Token.IDENTIFIER ] <&> VariableE
       , do tk <- singleMatching [ Token.STRING ]
            case tokenLiteral tk of
              Just (Token.LitString s) -> pure (StringE s)
              _ -> fancyFailure $ makeError (Just tk) "SCANNER ERROR: Expected string literal in STRING token."
       , do tk <- singleMatching [ Token.NUMBER ]
            case tokenLiteral tk of
              Just (Token.LitNum n) -> pure (NumE n)
              _ -> fancyFailure $ makeError (Just tk) "SCANNER ERROR: Expected numeric literal in NUMBER token."
       , do singleMatching [ Token.LEFT_PAREN ]
            expr <- expression
            consume [ Token.RIGHT_PAREN ] "Expect ')' after expression."
            pure $ GroupingE expr
       , do tk <- lookAhead maybeAny
            fancyFailure (makeError tk "Expect expression.")
       ]

leftAssociativeBinaryOp :: Foldable t
                        => MonadParsec ParserError TokenStream m
                        => (Expr -> Token -> Expr -> Expr)
                        -> m Expr
                        -> t TokenType
                        -> m Expr
leftAssociativeBinaryOp makeExpr termParser opTypes = do
  left <- termParser
  following <- many ((,) <$> singleMatching opTypes
                         <*> termParser)
  pure $ foldl' (\l (op, r) -> makeExpr l op r) left following

singleMatching :: Foldable t
               => MonadParsec ParserError TokenStream m
               => t TokenType -> m Token
singleMatching tkTypes = try $ satisfy (\tk -> any ((tokenType tk) ==) tkTypes)

maybeAny :: MonadParsec ParserError TokenStream m => m (Maybe Token)
maybeAny = (Just <$> anySingle) <|> pure Nothing

consume :: Foldable t
        => MonadParsec ParserError TokenStream m
        => t TokenType -> T.Text -> m Token
consume tkTypes errorMsg = do
  singleMatching tkTypes <|> do
    mbTk <- lookAhead maybeAny
    fancyFailure (makeError mbTk errorMsg)

checkForKnownErrorProductions
  :: Foldable t
  => MonadParsec ParserError TokenStream m
  => t (m a) -> m a
checkForKnownErrorProductions errorProductions
  = lookAhead $ asum errorProductions

binaryOperatorAtBeginningOfExpression
  :: Foldable t
  => MonadParsec ParserError TokenStream m
  => m a -> t TokenType -> m b
binaryOperatorAtBeginningOfExpression termParser opTypes = do
  op <- singleMatching opTypes
  observing termParser
  fancyFailure $ makeError (Just op) $ "Binary operator "
                                    <> (tokenLexeme op)
                                    <> " found at the beginning of expression."
