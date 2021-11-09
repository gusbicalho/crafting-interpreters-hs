module HSLox.Parser.Megaparsec where

import Control.Algebra (Has)
import Control.Effect.Writer (Writer)
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Data.Foldable (Foldable (foldl'), asum, for_)
import Data.Functor (void, ($>), (<&>))
import Data.Functor.Identity (Identity (Identity))
import Data.Maybe (catMaybes)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import HSLox.AST qualified as AST
import HSLox.AST.Sugar qualified as AST.Sugar
import HSLox.Parser.Megaparsec.TokenStream (TokenStream (..))
import HSLox.Parser.ParserError (ParserError (..))
import HSLox.Parser.ParserError qualified as ParserError
import HSLox.Token (Token (..), TokenType)
import HSLox.Token qualified as Token
import Text.Megaparsec hiding (State, Token)

parse ::
  Has (Writer (Set.Set ParserError)) sig m =>
  Seq Token ->
  m AST.ProgramI
parse tokens = do
  stmts' <- runParserT parseStmts "" (TokenStream tokens)
  case stmts' of
    Left errorBundle -> do
      for_ (bundleErrors errorBundle) report
      pure $ AST.Program Seq.empty
    Right stmts -> pure $ AST.Program stmts
 where
  parseStmts = do
    stmts <- manyStmtsUntilEOF (lift . report)
    reportAndClearDelayedErrors
    pure stmts
  reportAndClearDelayedErrors = do
    state <- getParserState
    let delayedErrors = stateParseErrors state
    updateParserState $ \state -> state{stateParseErrors = []}
    for_ delayedErrors (lift . report)
  report (FancyError _ errs) =
    for_ errs $ \case
      ErrorCustom e -> ParserError.reportError e
      err -> ParserError.reportError $ ParserError Nothing (T.pack $ show err)
  report err =
    ParserError.reportError $ ParserError Nothing (T.pack $ show err)

manyStmtsUntilEOF ::
  MonadParsec ParserError TokenStream m =>
  (ParseError TokenStream ParserError -> m ()) ->
  m (Seq AST.StmtI)
manyStmtsUntilEOF handleError =
  Seq.fromList . catMaybes
    <$> manyTill
      (withRecovery recover (Just <$> declaration))
      (eof <|> void (singleMatching [Token.EOF]))
 where
  recover err = do
    handleError err
    skipManyTill anySingle (synchronizationPoint <|> eof)
    pure Nothing
  synchronizationPoint = try $ do
    tk <- anySingle
    when (tokenType tk /= Token.SEMICOLON) $
      void . lookAhead . singleMatching $
        [ Token.CLASS
        , Token.FUN
        , Token.VAR
        , Token.FOR
        , Token.IF
        , Token.WHILE
        , Token.RETURN
        ]

makeError :: Maybe Token -> T.Text -> Set.Set (ErrorFancy ParserError)
makeError mbTk msg = Set.singleton . ErrorCustom $ ParserError mbTk msg

declaration :: MonadParsec ParserError TokenStream m => m AST.StmtI
declaration =
  asum
    [ varDeclaration
    , funDeclaration
    , classDeclaration
    , statement
    ]

varDeclaration :: MonadParsec ParserError TokenStream m => m AST.StmtI
varDeclaration = do
  singleMatching [Token.VAR]
  identifier <- consume [Token.IDENTIFIER] "Expect variable name."
  init <-
    asum
      [ singleMatching [Token.EQUAL] *> expression
      , pure AST.NilE
      ]
  consume [Token.SEMICOLON] "Expect ';' after variable declaration."
  pure . AST.VarDeclarationStmtI $ AST.VarDeclaration identifier init

funDeclaration :: MonadParsec ParserError TokenStream m => m AST.StmtI
funDeclaration = do
  singleMatching [Token.FUN]
  (name, function) <- function "function" parseFunName
  pure $ AST.FunDeclarationStmtI $ AST.FunDeclaration name function
 where
  parseFunName =
    consume [Token.IDENTIFIER] "Expect function name." <&> \functionName ->
      FunctionExprIdentifier functionName (Just functionName)

classDeclaration :: MonadParsec ParserError TokenStream m => m AST.StmtI
classDeclaration = do
  singleMatching [Token.CLASS]
  className <- consume [Token.IDENTIFIER] "Expect class name."
  superclass <-
    ( do
        singleMatching [Token.LESS]
        tk <- consume [Token.IDENTIFIER] "Expect superclass name."
        pure . Just . Identity . AST.Variable $ tk
      )
      <|> pure Nothing
  consume [Token.LEFT_BRACE] "Expect '{' before class body."
  methods <- parseMethods Seq.empty
  consume [Token.RIGHT_BRACE] "Expect '}' after class body."
  pure $ AST.ClassDeclarationStmtI $ AST.ClassDeclaration className superclass methods
 where
  parseMethods acc = do
    endOfClass <- check [Token.RIGHT_BRACE, Token.EOF]
    if endOfClass
      then pure acc
      else do
        (_, method) <- function "method" parseMethodName
        parseMethods (acc :|> Identity method)
  parseMethodName =
    FunctionExprIdentifier
      <$> consume [Token.IDENTIFIER] "Expect method name."
      <*> pure Nothing

data FunctionExprIdentifier = FunctionExprIdentifier
  { functionExprMarker :: Token
  , functionExprRecursiveIdentifier :: Maybe Token
  }

function ::
  MonadParsec ParserError TokenStream m =>
  T.Text ->
  m FunctionExprIdentifier ->
  m (Token, AST.Function Identity)
function kind parseName = do
  FunctionExprIdentifier
    { functionExprMarker
    , functionExprRecursiveIdentifier
    } <-
    parseName
  consume [Token.LEFT_PAREN] $ "Expect '(' after " <> kind <> " name."
  args <- arguments
  consume [Token.RIGHT_PAREN] "Expect ')' after parameters."
  consume [Token.LEFT_BRACE] $ "Expect '{' before " <> kind <> " body."
  body <- finishBlock
  pure (functionExprMarker, AST.Function functionExprMarker functionExprRecursiveIdentifier args body)
 where
  arguments = do
    endOfArgsList <- check [Token.RIGHT_PAREN]
    if endOfArgsList
      then pure Seq.empty
      else argsList Seq.empty
  argsList args = do
    when (Seq.length args >= 255) $ do
      tk <- lookAhead maybeAny
      fancyFailure $ makeError tk "Cannot have more than 255 parameters."
    arg <- consume [Token.IDENTIFIER] "Expect parameter name."
    let newArgs = args :|> arg
    ( singleMatching [Token.COMMA]
        *> argsList newArgs
      )
      <|> pure newArgs

statement :: MonadParsec ParserError TokenStream m => m AST.StmtI
statement =
  asum
    [ blockStmt
    , ifStmt
    , whileStmt
    , forStmt
    , returnStmt
    , expressionStmt
    ]

ifStmt :: MonadParsec ParserError TokenStream m => m AST.StmtI
ifStmt = do
  singleMatching [Token.IF]
  consume [Token.LEFT_PAREN] "Expect '(' after 'if'."
  condition <- expression
  consume [Token.RIGHT_PAREN] "Expect ')' after if condition."
  thenStmt <- statement
  elseStmt <-
    do
      _ <- singleMatching [Token.ELSE]
      Just <$> statement
      <|> pure Nothing
  pure . AST.IfStmtI $ AST.If condition thenStmt elseStmt

whileStmt :: MonadParsec ParserError TokenStream m => m AST.StmtI
whileStmt = do
  singleMatching [Token.WHILE]
  consume [Token.LEFT_PAREN] "Expect '(' after 'while'."
  condition <- expression
  consume [Token.RIGHT_PAREN] "Expect ')' after while condition."
  body <- statement
  pure . AST.WhileStmtI $ AST.While condition body

forStmt :: MonadParsec ParserError TokenStream m => m AST.StmtI
forStmt = do
  singleMatching [Token.FOR]
  consume [Token.LEFT_PAREN] "Expect '(' after 'for'."
  AST.Sugar.buildFor <$> init
    <*> condition
    <*> increment
    <*> statement
 where
  init =
    asum
      [ singleMatching [Token.SEMICOLON] $> Nothing
      , Just <$> varDeclaration
      , Just <$> expressionStmt
      ]
  condition =
    asum
      [ singleMatching [Token.SEMICOLON] $> Nothing
      , Just <$> expression
          <* consume [Token.SEMICOLON] "Expect ';' after for condition."
      ]
  increment =
    asum
      [ singleMatching [Token.RIGHT_PAREN] $> Nothing
      , Just <$> expression
          <* consume [Token.RIGHT_PAREN] "Expect ')' after for increment."
      ]

blockStmt :: MonadParsec ParserError TokenStream m => m AST.StmtI
blockStmt = AST.BlockStmtI <$> (singleMatching [Token.LEFT_BRACE] *> finishBlock)

finishBlock :: MonadParsec ParserError TokenStream m => m (AST.Block Identity)
finishBlock = do
  stmts <- blockBody Seq.empty
  consume [Token.RIGHT_BRACE] "Expect '}' after block."
  pure . AST.Block $ stmts
 where
  blockBody stmts =
    do
      (void . lookAhead $ singleMatching [Token.RIGHT_BRACE, Token.EOF]) <|> eof
      pure stmts
      <|> do
        stmt <- declaration
        blockBody (stmts Seq.:|> stmt)

returnStmt :: MonadParsec ParserError TokenStream m => m AST.StmtI
returnStmt = do
  returnTk <- singleMatching [Token.RETURN]
  (AST.ReturnStmtI (AST.Return returnTk Nothing) <$ singleMatching [Token.SEMICOLON])
    <|> ( do
            expr <- expression
            consume [Token.SEMICOLON] "Expect ';' after expression."
            pure . AST.ReturnStmtI $ AST.Return returnTk (Just expr)
        )

expressionStmt :: MonadParsec ParserError TokenStream m => m AST.StmtI
expressionStmt = do
  expr <- expression
  consume [Token.SEMICOLON] "Expect ';' after expression."
  pure $ AST.ExprStmtI expr

expression :: MonadParsec ParserError TokenStream m => m AST.ExprI
expression = comma

comma :: MonadParsec ParserError TokenStream m => m AST.ExprI
comma =
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression assignment opTypes]
    <|> leftAssociativeBinaryOp AST.BinaryE assignment opTypes
 where
  opTypes = [Token.COMMA]

assignment :: MonadParsec ParserError TokenStream m => m AST.ExprI
assignment = do
  left <- conditional
  assignTo left <|> pure left
 where
  assignTo left = do
    equals <- singleMatching [Token.EQUAL]
    right <- assignment
    case left of
      AST.VariableE tk -> pure $ AST.AssignmentE tk right
      AST.GetPropertyE obj prop -> pure $ AST.SetPropertyE obj prop right
      _ -> do
        registerFancyFailure (makeError (Just equals) "Invalid assignment target.")
        pure left

conditional :: MonadParsec ParserError TokenStream m => m AST.ExprI
conditional = do
  left <- orExpr
  conditionalBody left <|> pure left
 where
  conditionalBody left =
    AST.TernaryE left
      <$> singleMatching [Token.QUESTION_MARK]
      <*> expression
      <*> consume [Token.COLON] "Expect ':' after '?' expression."
      <*> conditional

orExpr :: MonadParsec ParserError TokenStream m => m AST.ExprI
orExpr =
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression andExpr opTypes]
    <|> leftAssociativeBinaryOp AST.LogicalE andExpr opTypes
 where
  opTypes = [Token.OR]

andExpr :: MonadParsec ParserError TokenStream m => m AST.ExprI
andExpr =
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression equality opTypes]
    <|> leftAssociativeBinaryOp AST.LogicalE equality opTypes
 where
  opTypes = [Token.AND]

equality :: MonadParsec ParserError TokenStream m => m AST.ExprI
equality =
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression comparison opTypes]
    <|> leftAssociativeBinaryOp AST.BinaryE comparison opTypes
 where
  opTypes =
    [ Token.EQUAL_EQUAL
    , Token.BANG_EQUAL
    ]

comparison :: MonadParsec ParserError TokenStream m => m AST.ExprI
comparison =
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression addition opTypes]
    <|> leftAssociativeBinaryOp AST.BinaryE addition opTypes
 where
  opTypes =
    [ Token.GREATER
    , Token.GREATER_EQUAL
    , Token.LESS
    , Token.LESS_EQUAL
    ]

addition :: MonadParsec ParserError TokenStream m => m AST.ExprI
addition =
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression multiplication [Token.PLUS]]
    <|> leftAssociativeBinaryOp
      AST.BinaryE
      multiplication
      [ Token.MINUS
      , Token.PLUS
      ]

multiplication :: MonadParsec ParserError TokenStream m => m AST.ExprI
multiplication =
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression unary opTypes]
    <|> leftAssociativeBinaryOp AST.BinaryE unary opTypes
 where
  opTypes =
    [ Token.STAR
    , Token.SLASH
    ]

unary :: MonadParsec ParserError TokenStream m => m AST.ExprI
unary =
  ( AST.UnaryE <$> singleMatching [Token.MINUS, Token.BANG]
      <*> unary
  )
    <|> call

call :: MonadParsec ParserError TokenStream m => m AST.ExprI
call = primary >>= sequenceOfCalls
 where
  sequenceOfCalls callee =
    ( do
        _ <- singleMatching [Token.LEFT_PAREN]
        args <- arguments
        paren <- consume [Token.RIGHT_PAREN] "Expect ')' after arguments."
        sequenceOfCalls (AST.CallE callee paren args)
    )
      <|> ( do
              _ <- singleMatching [Token.DOT]
              property <- consume [Token.IDENTIFIER] "Expect property name after '.'."
              sequenceOfCalls (AST.GetPropertyE callee property)
          )
      <|> pure callee
  arguments = do
    endOfArgsList <- check [Token.RIGHT_PAREN]
    if endOfArgsList
      then pure Seq.empty
      else argumentsList Seq.empty
  argumentsList args = do
    when (Seq.length args >= 255) $ do
      tk <- maybeAny
      registerFancyFailure $ makeError tk "Cannot have more than 255 arguments."
    args <- (args :|>) <$> assignment
    ( do
        _ <- singleMatching [Token.COMMA]
        argumentsList args
      )
      <|> pure args

primary :: MonadParsec ParserError TokenStream m => m AST.ExprI
primary =
  asum
    [ singleMatching [Token.FALSE] $> AST.BoolE False
    , singleMatching [Token.TRUE] $> AST.BoolE True
    , singleMatching [Token.NIL] $> AST.NilE
    , singleMatching [Token.IDENTIFIER] <&> AST.VariableE
    , singleMatching [Token.THIS] <&> AST.ThisE
    , singleMatching [Token.FUN] >>= inlineFunction
    , singleMatching [Token.SUPER] >>= \tk -> do
        consume [Token.DOT] "Expect '.' after 'super'."
        property <- consume [Token.IDENTIFIER] "Expect superclass method name."
        pure (AST.SuperE tk property)
    , singleMatching [Token.STRING] >>= \tk ->
        case tokenLiteral tk of
          Just (Token.LitString s) -> pure (AST.StringE s)
          _ -> fancyFailure $ makeError (Just tk) "SCANNER ERROR: Expected string literal in STRING token."
    , singleMatching [Token.NUMBER] >>= \tk ->
        case tokenLiteral tk of
          Just (Token.LitNum n) -> pure (AST.NumE n)
          _ -> fancyFailure $ makeError (Just tk) "SCANNER ERROR: Expected numeric literal in NUMBER token."
    , singleMatching [Token.LEFT_PAREN] *> do
        expr <- expression
        consume [Token.RIGHT_PAREN] "Expect ')' after expression."
        pure $ AST.GroupingE expr
    , lookAhead maybeAny >>= \tk ->
        fancyFailure (makeError tk "Expect expression.")
    ]

inlineFunction :: MonadParsec ParserError TokenStream m => Token -> m AST.ExprI
inlineFunction marker = do
  (_, function) <- function "function" (parseFunName <|> anonymousFunName)
  pure $ AST.FunctionExprI function
 where
  parseFunName =
    singleMatching [Token.IDENTIFIER] <&> \functionName ->
      FunctionExprIdentifier functionName (Just functionName)
  anonymousFunName = pure $ FunctionExprIdentifier marker Nothing

leftAssociativeBinaryOp ::
  Foldable t =>
  MonadParsec ParserError TokenStream m =>
  (AST.ExprI -> Token -> AST.ExprI -> AST.ExprI) ->
  m AST.ExprI ->
  t TokenType ->
  m AST.ExprI
leftAssociativeBinaryOp makeExpr termParser opTypes = do
  left <- termParser
  following <-
    many
      ( (,) <$> singleMatching opTypes
          <*> termParser
      )
  pure $ foldl' (\l (op, r) -> makeExpr l op r) left following

singleMatching ::
  Foldable t =>
  MonadParsec ParserError TokenStream m =>
  t TokenType ->
  m Token
singleMatching tkTypes = try $ satisfy (\tk -> tokenType tk `elem` tkTypes)

check :: (Foldable t, MonadParsec ParserError TokenStream m) => t TokenType -> m Bool
check tkTypes =
  lookAhead $
    (True <$ singleMatching tkTypes)
      <|> pure False

maybeAny :: MonadParsec ParserError TokenStream m => m (Maybe Token)
maybeAny = optional anySingle

consume ::
  Foldable t =>
  MonadParsec ParserError TokenStream m =>
  t TokenType ->
  T.Text ->
  m Token
consume tkTypes errorMsg =
  singleMatching tkTypes <|> do
    mbTk <- lookAhead maybeAny
    fancyFailure (makeError mbTk errorMsg)

checkForKnownErrorProductions ::
  Foldable t =>
  MonadParsec ParserError TokenStream m =>
  t (m a) ->
  m a
checkForKnownErrorProductions errorProductions =
  lookAhead $ asum errorProductions

binaryOperatorAtBeginningOfExpression ::
  Foldable t =>
  MonadParsec ParserError TokenStream m =>
  m a ->
  t TokenType ->
  m b
binaryOperatorAtBeginningOfExpression termParser opTypes = do
  op <- singleMatching opTypes
  observing termParser
  fancyFailure $
    makeError (Just op) $
      "Binary operator "
        <> tokenLexeme op
        <> " found at the beginning of expression."
