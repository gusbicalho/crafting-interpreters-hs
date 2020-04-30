module HSLox.Parser.Megaparsec where

import Control.Effect.Error as ErrorEff
import Control.Carrier.Writer.Church
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Data.Maybe (catMaybes)
import Data.Foldable
import Data.Functor
import Data.Sequence (Seq (..))
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

parse :: Has (Writer (Set.Set ParserError)) sig m
      => Seq Token
      -> m ProgramI
parse tokens = do
    stmts' <- runParserT parseStmts "" (TokenStream tokens)
    case stmts' of
      Left errorBundle -> do
        for_ (bundleErrors errorBundle) report
        pure $ Program Seq.empty
      Right stmts -> pure $ Program stmts
  where
    parseStmts = do
      stmts <- manyStmtsUntilEOF (lift . report)
      reportAndClearDelayedErrors
      pure stmts
    reportAndClearDelayedErrors = do
      state <- getParserState
      let delayedErrors = stateParseErrors state
      updateParserState $ \state -> state { stateParseErrors = [] }
      for_ delayedErrors (lift . report)
    report (FancyError _ errs) =
      for_ errs $ \case
        ErrorCustom e -> ParserError.reportError e
        err -> ParserError.reportError $ ParserError Nothing (T.pack $ show err)
    report err =
      ParserError.reportError $ ParserError Nothing (T.pack $ show err)

manyStmtsUntilEOF :: MonadParsec ParserError TokenStream m
                  => (ParseError TokenStream ParserError -> m ())
                  -> m (Seq StmtI)
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
                                            , Token.RETURN
                                            ]

makeError :: Maybe Token -> T.Text -> Set.Set (ErrorFancy ParserError)
makeError mbTk msg = Set.singleton . ErrorCustom $ ParserError mbTk msg

declaration :: MonadParsec ParserError TokenStream m => m StmtI
declaration = asum [ varDeclaration
                   , funDeclaration
                   , classDeclaration
                   , statement
                   ]

varDeclaration :: MonadParsec ParserError TokenStream m => m StmtI
varDeclaration = do
    singleMatching [Token.VAR]
    identifier <- consume [Token.IDENTIFIER] "Expect variable name."
    init <- asum [ singleMatching [Token.EQUAL] *> expression
                 , pure NilE
                 ]
    consume [Token.SEMICOLON] "Expect ';' after variable declaration."
    pure . VarDeclarationStmtI $ VarDeclaration identifier init

funDeclaration :: MonadParsec ParserError TokenStream m => m StmtI
funDeclaration = do
    singleMatching [Token.FUN]
    (name, function) <- function "function" parseFunName
    pure $ FunDeclarationStmtI $ FunDeclaration name function
  where
    parseFunName = consume [Token.IDENTIFIER] "Expect function name."

classDeclaration :: MonadParsec ParserError TokenStream m => m StmtI
classDeclaration = do
    singleMatching [Token.CLASS]
    className <- consume [Token.IDENTIFIER] "Expect class name."
    consume [Token.LEFT_BRACE] "Expect '{' before class body."
    methods <- parseMethods Seq.empty
    consume [Token.RIGHT_BRACE] "Expect '}' after class body."
    pure $ ClassDeclarationStmtI $ ClassDeclaration className methods
  where
    parseMethods acc = do
      endOfClass <- check [Token.RIGHT_BRACE, Token.EOF]
      if endOfClass
      then pure acc
      else do
        (_, method) <- function "method" parseMethodName
        parseMethods (acc :|> method)
    parseMethodName = consume [Token.IDENTIFIER] $ "Expect method name."

function :: MonadParsec ParserError TokenStream m
         => T.Text -> m Token -> m (Token, Function Identity)
function kind parseName = do
    name <- parseName
    consume [Token.LEFT_PAREN] $ "Expect '(' after " <> kind <> " name."
    args <- arguments
    consume [Token.RIGHT_PAREN] "Expect ')' after parameters."
    consume [Token.LEFT_BRACE] $ "Expect '{' before " <> kind <> " body."
    body <- finishBlock
    pure (name, Function name args body)
  where
    arguments = do
      endOfArgsList <- check [ Token.RIGHT_PAREN ]
      if endOfArgsList
      then pure Seq.empty
      else argsList Seq.empty
    argsList args = do
      when (Seq.length args >= 255) $ do
        tk <- lookAhead maybeAny
        fancyFailure $ makeError tk "Cannot have more than 255 parameters."
      arg <- consume [Token.IDENTIFIER] "Expect parameter name."
      let newArgs = args :|> arg
      (singleMatching [Token.COMMA]
        *> argsList newArgs)
        <|> pure newArgs

statement :: MonadParsec ParserError TokenStream m => m StmtI
statement = asum [ blockStmt
                 , ifStmt
                 , whileStmt
                 , forStmt
                 , returnStmt
                 , expressionStmt
                 ]

ifStmt :: MonadParsec ParserError TokenStream m => m StmtI
ifStmt = do
  singleMatching [ Token.IF ]
  consume [ Token.LEFT_PAREN ] "Expect '(' after 'if'."
  condition <- expression
  consume [ Token.RIGHT_PAREN ] "Expect ')' after if condition."
  thenStmt <- statement
  elseStmt <- do _ <- singleMatching [ Token.ELSE ]
                 Just <$> statement
              <|> pure Nothing
  pure . IfStmtI $ If condition thenStmt elseStmt

whileStmt :: MonadParsec ParserError TokenStream m => m StmtI
whileStmt = do
  singleMatching [ Token.WHILE ]
  consume [ Token.LEFT_PAREN ] "Expect '(' after 'while'."
  condition <- expression
  consume [ Token.RIGHT_PAREN ] "Expect ')' after while condition."
  body <- statement
  pure . WhileStmtI $ While condition body

forStmt :: MonadParsec ParserError TokenStream m => m StmtI
forStmt = do
    singleMatching [ Token.FOR ]
    consume [ Token.LEFT_PAREN ] "Expect '(' after 'for'."
    buildFor <$> init
             <*> condition
             <*> increment
             <*> statement
  where
    init      = asum [ singleMatching [ Token.SEMICOLON ] $> Nothing
                     , Just <$> varDeclaration
                     , Just <$> expressionStmt
                     ]
    condition = asum [ singleMatching [ Token.SEMICOLON ] $> Nothing
                     , Just <$> expression
                        <* consume [ Token.SEMICOLON ] "Expect ';' after for condition."
                     ]
    increment = asum [ singleMatching [ Token.RIGHT_PAREN ] $> Nothing
                     , Just <$> expression
                         <* consume [ Token.RIGHT_PAREN ] "Expect ')' after for increment."
                     ]


blockStmt :: MonadParsec ParserError TokenStream m => m StmtI
blockStmt = BlockStmtI <$> (singleMatching [ Token.LEFT_BRACE ] *> finishBlock)

finishBlock :: MonadParsec ParserError TokenStream m => m (Block Identity)
finishBlock = do
    stmts <- blockBody Seq.empty
    consume [ Token.RIGHT_BRACE ] "Expect '}' after block."
    pure . Block $ stmts
  where
    blockBody stmts =
      do (void . lookAhead $ singleMatching [ Token.RIGHT_BRACE, Token.EOF ]) <|> eof
         pure stmts
      <|> do stmt <- declaration
             blockBody (stmts Seq.:|> stmt)

returnStmt :: MonadParsec ParserError TokenStream m => m StmtI
returnStmt = do
  returnTk <- singleMatching [ Token.RETURN ]
  expr <- (NilE <$ singleMatching [Token.SEMICOLON])
          <|> (do expr <- expression
                  consume [Token.SEMICOLON] "Expect ';' after expression."
                  pure expr)
  pure . ReturnStmtI $ Return returnTk expr

expressionStmt :: MonadParsec ParserError TokenStream m => m StmtI
expressionStmt = do
  expr <- expression
  consume [ Token.SEMICOLON ] "Expect ';' after expression."
  pure $ ExprStmtI expr

expression :: MonadParsec ParserError TokenStream m => m ExprI
expression = comma

comma :: MonadParsec ParserError TokenStream m => m ExprI
comma =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression assignment opTypes ]
      <|> leftAssociativeBinaryOp BinaryE assignment opTypes
  where
    opTypes = [ Token.COMMA ]

assignment :: MonadParsec ParserError TokenStream m => m ExprI
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
          pure left

conditional :: MonadParsec ParserError TokenStream m => m ExprI
conditional = do
    left <- orExpr
    conditionalBody left <|> pure left
  where
    conditionalBody left =
      TernaryE left
               <$> singleMatching [ Token.QUESTION_MARK ]
               <*> expression
               <*> consume [ Token.COLON ] "Expect ':' after '?' expression."
               <*> conditional

orExpr :: MonadParsec ParserError TokenStream m => m ExprI
orExpr =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression andExpr opTypes ]
      <|> leftAssociativeBinaryOp LogicalE andExpr opTypes
  where
    opTypes = [ Token.OR ]

andExpr :: MonadParsec ParserError TokenStream m => m ExprI
andExpr =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression equality opTypes ]
      <|> leftAssociativeBinaryOp LogicalE equality opTypes
  where
    opTypes = [ Token.AND ]

equality :: MonadParsec ParserError TokenStream m => m ExprI
equality =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression comparison opTypes ]
      <|> leftAssociativeBinaryOp BinaryE comparison opTypes
  where
    opTypes = [ Token.EQUAL_EQUAL
              , Token.BANG_EQUAL
              ]

comparison :: MonadParsec ParserError TokenStream m => m ExprI
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

addition :: MonadParsec ParserError TokenStream m => m ExprI
addition =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression multiplication [ Token.PLUS ] ]
      <|> leftAssociativeBinaryOp BinaryE multiplication [ Token.MINUS
                                                         , Token.PLUS
                                                         ]

multiplication :: MonadParsec ParserError TokenStream m => m ExprI
multiplication =
    checkForKnownErrorProductions
      [ binaryOperatorAtBeginningOfExpression unary opTypes ]
      <|> leftAssociativeBinaryOp BinaryE unary opTypes
  where
    opTypes = [ Token.STAR
              , Token.SLASH
              ]

unary :: MonadParsec ParserError TokenStream m => m ExprI
unary = (UnaryE <$> singleMatching [Token.MINUS, Token.BANG]
                <*> unary)
    <|> call

call :: MonadParsec ParserError TokenStream m => m ExprI
call = primary >>= sequenceOfCalls
  where
    sequenceOfCalls callee =
      (do _ <- singleMatching [ Token.LEFT_PAREN ]
          args <- arguments
          paren <- consume [ Token.RIGHT_PAREN ] "Expect ')' after arguments."
          sequenceOfCalls (CallE callee paren args))
      <|> (do _ <- singleMatching [ Token.DOT ]
              property <- consume [Token.IDENTIFIER] "Expect property name after '.'."
              sequenceOfCalls (GetE callee property))
      <|> pure callee
    arguments = do
      endOfArgsList <- check [ Token.RIGHT_PAREN ]
      if endOfArgsList
      then pure Seq.empty
      else argumentsList Seq.empty
    argumentsList args = do
      when (Seq.length args >= 255) $ do
        tk <- maybeAny
        registerFancyFailure $ makeError tk "Cannot have more than 255 arguments."
      args <- (args :|>) <$> assignment
      (do _ <- singleMatching [ Token.COMMA ]
          argumentsList args)
        <|> pure args

primary :: MonadParsec ParserError TokenStream m => m ExprI
primary =
  asum [ singleMatching [ Token.FALSE ]       $> BoolE False
       , singleMatching [ Token.TRUE ]        $> BoolE True
       , singleMatching [ Token.NIL ]         $> NilE
       , singleMatching [ Token.IDENTIFIER ] <&> VariableE
       , singleMatching [ Token.FUN ]        >>= anonymousFunction
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

anonymousFunction :: MonadParsec ParserError TokenStream m => Token -> m ExprI
anonymousFunction marker = do
  (_, function) <- function "function" (pure marker)
  pure $ FunctionExprI function

leftAssociativeBinaryOp :: Foldable t
                        => MonadParsec ParserError TokenStream m
                        => (ExprI -> Token -> ExprI -> ExprI)
                        -> m ExprI
                        -> t TokenType
                        -> m ExprI
leftAssociativeBinaryOp makeExpr termParser opTypes = do
  left <- termParser
  following <- many ((,) <$> singleMatching opTypes
                         <*> termParser)
  pure $ foldl' (\l (op, r) -> makeExpr l op r) left following

singleMatching :: Foldable t
               => MonadParsec ParserError TokenStream m
               => t TokenType -> m Token
singleMatching tkTypes = try $ satisfy (\tk -> tokenType tk `elem` tkTypes)

check :: (Foldable t, MonadParsec ParserError TokenStream m) => t TokenType -> m Bool
check tkTypes = lookAhead $ (True <$ singleMatching tkTypes)
                        <|> pure False

maybeAny :: MonadParsec ParserError TokenStream m => m (Maybe Token)
maybeAny = (Just <$> anySingle) <|> pure Nothing

consume :: Foldable t
        => MonadParsec ParserError TokenStream m
        => t TokenType -> T.Text -> m Token
consume tkTypes errorMsg =
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
                                    <> tokenLexeme op
                                    <> " found at the beginning of expression."
