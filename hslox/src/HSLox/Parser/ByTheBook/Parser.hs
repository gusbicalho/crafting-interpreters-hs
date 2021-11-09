module HSLox.Parser.ByTheBook.Parser where

import Control.Algebra (Has)
import Control.Carrier.Empty.Church qualified as Empty.Church
import Control.Carrier.State.Church qualified as State.Church
import Control.Carrier.Writer.Church qualified as Writer.Church
import Control.Effect.Empty (Empty)
import Control.Effect.Empty qualified as Empty
import Control.Effect.Error qualified as ErrorEff
import Control.Effect.State (State)
import Control.Effect.State qualified as State
import Control.Effect.Writer (Writer)
import Control.Effect.Writer qualified as Writer
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Functor (void, ($>), (<&>))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Text qualified as T
import HSLox.AST qualified as AST
import HSLox.AST.Sugar qualified as AST.Sugar
import HSLox.Parser.ByTheBook.ParserState (ParserState)
import HSLox.Parser.ByTheBook.ParserState qualified as ParserState
import HSLox.Parser.ParserError (ParserError (ParserError))
import HSLox.Parser.ParserError qualified as ParserError
import HSLox.Token (Token (..), TokenType)
import HSLox.Token qualified as Token
import HSLox.Util qualified as Util
import HSLox.AST.Meta (pattern NoMeta)

parse ::
  Has (Writer (Set ParserError)) sig m =>
  Seq Token ->
  m AST.ProgramI
parse tokens =
  State.Church.evalState (ParserState.initialParserState tokens)
    . fmap AST.Program
    . Writer.Church.execWriter @(Seq AST.StmtI)
    . Util.untilEmpty
    $ do
      Empty.guard . not =<< ParserState.isAtEnd
      stmt' <- Util.runErrorToEither @ParserError declaration
      case stmt' of
        Left error -> do
          ParserError.reportError error
          synchronize
        Right stmt -> do
          Writer.tell $ Seq.singleton stmt

type LoxParser t sig m =
  ( Has (Writer (Set ParserError)) sig m
  , Has (ErrorEff.Error ParserError) sig m
  , Has (State ParserState) sig m
  ) =>
  m t

type ExprParser sig m = LoxParser AST.ExprI sig m

type StmtParser sig m = LoxParser AST.StmtI sig m

synchronize :: Has (State ParserState) sig m => m ()
synchronize = Util.untilEmpty $ do
  tk <- ParserState.advance
  Empty.guard $ tokenType tk /= Token.SEMICOLON
  tk <- ParserState.peek
  Empty.guard $
    tokenType tk
      `notElem` [ Token.CLASS
                , Token.FUN
                , Token.VAR
                , Token.FOR
                , Token.IF
                , Token.WHILE
                , Token.RETURN
                ]

declaration :: StmtParser sig m
declaration =
  do
    varDeclaration
    `Util.recoverFromEmptyWith` funDeclaration
    `Util.recoverFromEmptyWith` classDeclaration
    `Util.recoverFromEmptyWith` statement

varDeclaration :: Has Empty sig m => StmtParser sig m
varDeclaration = do
  ParserState.match [Token.VAR]
  identifier <- consume [Token.IDENTIFIER] "Expect variable name."
  init <-
    (ParserState.match [Token.EQUAL] *> expression)
      `Util.recoverFromEmptyWith` pure AST.NilE
  consume [Token.SEMICOLON] "Expect ';' after variable declaration."
  pure . AST.VarDeclarationStmtI $ AST.VarDeclaration identifier init

funDeclaration :: Has Empty sig m => StmtParser sig m
funDeclaration = do
  ParserState.match [Token.FUN]
  (name, function) <- function "function" parseFunName
  pure $ AST.FunDeclarationStmtI $ AST.FunDeclaration name function
 where
  parseFunName =
    consume [Token.IDENTIFIER] "Expect function name." <&> \functionName ->
      FunctionExprIdentifier functionName (Just functionName)

classDeclaration :: Has Empty sig m => StmtParser sig m
classDeclaration = do
  ParserState.match [Token.CLASS]
  className <- consume [Token.IDENTIFIER] "Expect class name."
  superclass <- Util.runEmptyToMaybe $ do
    ParserState.match [Token.LESS]
    tk <- consume [Token.IDENTIFIER] "Expect superclass name."
    pure . NoMeta . AST.Variable $ tk
  consume [Token.LEFT_BRACE] "Expect '{' before class body."
  methods <- parseMethods Seq.empty
  consume [Token.RIGHT_BRACE] "Expect '}' after class body."
  pure $ AST.ClassDeclarationStmtI $ AST.ClassDeclaration className superclass methods
 where
  parseMethods acc = do
    endOfClass <- ParserState.check [Token.RIGHT_BRACE, Token.EOF]
    if endOfClass
      then pure acc
      else do
        (_, method) <- function "method" parseMethodName
        parseMethods (acc :|> NoMeta method)
  parseMethodName =
    FunctionExprIdentifier
      <$> consume [Token.IDENTIFIER] "Expect method name."
      <*> pure Nothing

data FunctionExprIdentifier = FunctionExprIdentifier
  { functionExprMarker :: Token
  , functionExprRecursiveIdentifier :: Maybe Token
  }

function :: T.Text -> LoxParser FunctionExprIdentifier sig m -> LoxParser (Token, AST.Function ()) sig m
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
    endOfArgsList <- ParserState.check [Token.RIGHT_PAREN]
    if endOfArgsList
      then pure Seq.empty
      else argsList Seq.empty
  argsList args = do
    when (Seq.length args >= 255) $ do
      tk <- Util.runEmptyToMaybe ParserState.peek
      ParserError.reportError $ ParserError tk "Cannot have more than 255 parameters."
    arg <- consume [Token.IDENTIFIER] "Expect parameter name."
    let newArgs = args :|> arg
    comma <- Util.runEmptyToMaybe $ ParserState.match [Token.COMMA]
    case comma of
      Just _ -> argsList newArgs
      Nothing -> pure newArgs

statement :: StmtParser sig m
statement =
  blockStmt
    `Util.recoverFromEmptyWith` ifStmt
    `Util.recoverFromEmptyWith` whileStmt
    `Util.recoverFromEmptyWith` forStmt
    `Util.recoverFromEmptyWith` returnStmt
    `Util.recoverFromEmptyWith` expressionStmt

blockStmt :: Has Empty sig m => StmtParser sig m
blockStmt = AST.BlockStmtI <$> (ParserState.match [Token.LEFT_BRACE] *> finishBlock)

finishBlock :: LoxParser (AST.Block ()) sig m
finishBlock = do
  stmts <- blockBody Seq.empty
  consume [Token.RIGHT_BRACE] "Expect '}' after block."
  pure $ AST.Block stmts
 where
  blockBody stmts = do
    endOfBlock <- ParserState.check [Token.RIGHT_BRACE, Token.EOF]
    if endOfBlock
      then pure stmts
      else do
        stmt <- declaration
        blockBody (stmts :|> stmt)

ifStmt :: Has Empty sig m => StmtParser sig m
ifStmt = do
  ParserState.match [Token.IF]
  consume [Token.LEFT_PAREN] "Expect '(' after 'if'."
  condition <- expression
  consume [Token.RIGHT_PAREN] "Expect ')' after if condition."
  thenStmt <- statement
  elseStmt <- Util.runEmptyToMaybe $ do
    _ <- ParserState.match [Token.ELSE]
    statement
  pure . AST.IfStmtI $ AST.If condition thenStmt elseStmt

whileStmt :: Has Empty sig m => StmtParser sig m
whileStmt = do
  ParserState.match [Token.WHILE]
  consume [Token.LEFT_PAREN] "Expect '(' after 'while'."
  condition <- expression
  consume [Token.RIGHT_PAREN] "Expect ')' after while condition."
  body <- statement
  pure . AST.WhileStmtI $ AST.While condition body

forStmt :: Has Empty sig m => StmtParser sig m
forStmt = do
  ParserState.match [Token.FOR]
  consume [Token.LEFT_PAREN] "Expect '(' after 'for'."
  AST.Sugar.buildFor <$> init <*> condition <*> increment <*> statement
 where
  init =
    (ParserState.match [Token.SEMICOLON] $> Nothing)
      `Util.recoverFromEmptyWith` (Just <$> varDeclaration)
      `Util.recoverFromEmptyWith` (Just <$> expressionStmt)
  condition =
    (ParserState.match [Token.SEMICOLON] $> Nothing)
      `Util.recoverFromEmptyWith` (Just <$> expression <* consume [Token.SEMICOLON] "Expect ';' after loop condition.")
  increment =
    (ParserState.match [Token.RIGHT_PAREN] $> Nothing)
      `Util.recoverFromEmptyWith` (Just <$> expression <* consume [Token.RIGHT_PAREN] "Expect ')' after for clauses.")

returnStmt :: Has Empty sig m => StmtParser sig m
returnStmt = do
  returnTk <- ParserState.match [Token.RETURN]
  (AST.ReturnStmtI (AST.Return returnTk Nothing) <$ ParserState.match [Token.SEMICOLON])
    `Util.recoverFromEmptyWith` ( do
                                    expr <- expression
                                    consume [Token.SEMICOLON] "Expect ';' after expression."
                                    pure . AST.ReturnStmtI $ AST.Return returnTk (Just expr)
                                )

expressionStmt :: StmtParser sig m
expressionStmt = do
  expr <- expression
  consume [Token.SEMICOLON] "Expect ';' after expression."
  pure $ AST.ExprStmtI expr

expression :: ExprParser sig m
expression = comma

comma :: ExprParser sig m
comma = do
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression assignment opTypes]
  leftAssociativeBinaryOp AST.BinaryE assignment opTypes
 where
  opTypes = [Token.COMMA]

assignment :: ExprParser sig m
assignment = do
  left <- conditional
  assignTo left
    `Util.recoverFromEmptyWith` pure left
 where
  assignTo left = do
    equals <- ParserState.match [Token.EQUAL]
    right <- assignment
    case left of
      AST.VariableE tk -> pure $ AST.AssignmentE tk right
      AST.GetPropertyE obj prop -> pure $ AST.SetPropertyE obj prop right
      _ -> do
        ParserError.reportError $ ParserError (Just equals) "Invalid assignment target."
        Empty.empty

conditional :: ExprParser sig m
conditional = do
  left <- orExpr
  mbTk <- Util.runEmptyToMaybe $ ParserState.match [Token.QUESTION_MARK]
  case mbTk of
    Nothing -> pure left
    Just op1 -> AST.TernaryE left op1 <$> middle <*> op2 <*> right
 where
  middle = expression
  op2 =
    ParserState.match [Token.COLON]
      `Util.recoverFromEmptyWith` throwParserError "Expect ':' after '?' expression."
  right = conditional

orExpr :: ExprParser sig m
orExpr = do
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression andExpr opTypes]
  leftAssociativeBinaryOp AST.LogicalE andExpr opTypes
 where
  opTypes = [Token.OR]

andExpr :: ExprParser sig m
andExpr = do
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression equality opTypes]
  leftAssociativeBinaryOp AST.LogicalE equality opTypes
 where
  opTypes = [Token.AND]

equality :: ExprParser sig m
equality = do
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression comparison opTypes]
  leftAssociativeBinaryOp AST.BinaryE comparison opTypes
 where
  opTypes =
    [ Token.EQUAL_EQUAL
    , Token.BANG_EQUAL
    ]

comparison :: ExprParser sig m
comparison = do
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression addition opTypes]
  leftAssociativeBinaryOp AST.BinaryE addition opTypes
 where
  opTypes =
    [ Token.GREATER
    , Token.GREATER_EQUAL
    , Token.LESS
    , Token.LESS_EQUAL
    ]

addition :: ExprParser sig m
addition = do
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression multiplication [Token.PLUS]]
  leftAssociativeBinaryOp
    AST.BinaryE
    multiplication
    [ Token.MINUS
    , Token.PLUS
    ]

multiplication :: ExprParser sig m
multiplication = do
  checkForKnownErrorProductions
    [binaryOperatorAtBeginningOfExpression unary opTypes]
  leftAssociativeBinaryOp AST.BinaryE unary opTypes
 where
  opTypes =
    [ Token.STAR
    , Token.SLASH
    ]

unary :: ExprParser sig m
unary = do
  mbTk <- Util.runEmptyToMaybe (ParserState.match [Token.MINUS, Token.BANG])
  case mbTk of
    Nothing -> call
    Just tk -> AST.UnaryE tk <$> unary

call :: ExprParser sig m
call = primary >>= sequenceOfCalls
 where
  sequenceOfCalls callee = do
    next <-
      Util.runEmptyToMaybe $
        ParserState.match
          [ Token.LEFT_PAREN
          , Token.DOT
          ]
    case next of
      Just tk
        | Token.LEFT_PAREN == tokenType tk -> do
          args <- arguments
          paren <- consume [Token.RIGHT_PAREN] "Expect ')' after arguments."
          sequenceOfCalls (AST.CallE callee paren args)
        | Token.DOT == tokenType tk -> do
          property <- consume [Token.IDENTIFIER] "Expect property name after '.'."
          sequenceOfCalls (AST.GetPropertyE callee property)
      _ -> pure callee
  arguments = do
    endOfArgsList <- ParserState.check [Token.RIGHT_PAREN]
    if endOfArgsList
      then pure Seq.empty
      else argumentsList Seq.empty
  argumentsList args = do
    when (Seq.length args >= 255) $ do
      tk <- Util.runEmptyToMaybe ParserState.peek
      ParserError.reportError $ ParserError tk "Cannot have more than 255 arguments."
    args <- (args :|>) <$> assignment
    followedByComma <- Util.runEmptyToBool $ ParserState.match [Token.COMMA]
    if followedByComma
      then argumentsList args
      else pure args

primary :: ExprParser sig m
primary = do
  mbTk <-
    Util.runEmptyToMaybe
      ( ParserState.match
          [ Token.FALSE
          , Token.TRUE
          , Token.NIL
          , Token.NUMBER
          , Token.STRING
          , Token.LEFT_PAREN
          , Token.IDENTIFIER
          , Token.THIS
          , Token.FUN
          , Token.SUPER
          ]
      )
  case mbTk of
    Just Token{tokenType = Token.FALSE} -> pure (AST.BoolE False)
    Just Token{tokenType = Token.TRUE} -> pure (AST.BoolE True)
    Just Token{tokenType = Token.NIL} -> pure AST.NilE
    Just tk@Token{tokenType = Token.IDENTIFIER} -> pure (AST.VariableE tk)
    Just tk@Token{tokenType = Token.THIS} -> pure (AST.ThisE tk)
    Just tk@Token{tokenType = Token.FUN} -> inlineFunction tk
    Just tk@Token{tokenType = Token.SUPER} -> do
      consume [Token.DOT] "Expect '.' after 'super'."
      property <- consume [Token.IDENTIFIER] "Expect superclass method name."
      pure (AST.SuperE tk property)
    Just Token{tokenType = Token.STRING, tokenLiteral} ->
      case tokenLiteral of
        Just (Token.LitString s) -> pure (AST.StringE s)
        _ -> throwParserError "SCANNER ERROR: Expected string literal in STRING token."
    Just Token{tokenType = Token.NUMBER, tokenLiteral} ->
      case tokenLiteral of
        Just (Token.LitNum n) -> pure (AST.NumE n)
        _ -> throwParserError "SCANNER ERROR: Expected numeric literal in NUMBER token."
    Just Token{tokenType = Token.LEFT_PAREN} -> do
      expr <- expression
      ParserState.match [Token.RIGHT_PAREN]
        `Util.recoverFromEmptyWith` throwParserError "Expect ')' after expression."
      pure $ AST.GroupingE expr
    _ -> throwParserError "Expect expression."

inlineFunction :: Token -> LoxParser AST.ExprI sig m
inlineFunction marker = do
  (_, function) <- function "function" (parseFunName `Util.recoverFromEmptyWith` anonymousFunName)
  pure $ AST.FunctionExprI function
 where
  parseFunName =
    ParserState.match [Token.IDENTIFIER] <&> \functionName ->
      FunctionExprIdentifier functionName (Just functionName)
  anonymousFunName = pure $ FunctionExprIdentifier marker Nothing

makeParserError ::
  Has (ErrorEff.Throw ParserError) sig m =>
  Has (State ParserState) sig m =>
  T.Text ->
  m ParserError
makeParserError msg = do
  tk <- ParserState.peek `Util.recoverFromEmptyWith` pure eof
  pure $ ParserError (Just tk) msg
 where
  eof = Token "" Token.EOF Nothing 0

throwParserError ::
  Has (ErrorEff.Throw ParserError) sig m =>
  Has (State ParserState) sig m =>
  T.Text ->
  m b
throwParserError msg = ErrorEff.throwError =<< makeParserError msg

leftAssociativeBinaryOp ::
  Foldable t =>
  (AST.ExprI -> Token -> AST.ExprI -> AST.ExprI) ->
  (forall sig m. ExprParser sig m) ->
  t TokenType ->
  ExprParser sig m
leftAssociativeBinaryOp makeExpr termParser opTypes = do
  left <- termParser
  State.Church.execState left . Util.untilEmpty $ do
    left <- State.get
    op <- ParserState.match opTypes
    right <- termParser
    State.put $ makeExpr left op right

checkForKnownErrorProductions ::
  Foldable t =>
  Has (State ParserState) sig m =>
  t (Empty.Church.EmptyC m a) ->
  m ()
checkForKnownErrorProductions errorProductions =
  Util.backingUpState @ParserState $ \restore -> do
    for_ errorProductions Util.runEmptyToUnit
    restore

binaryOperatorAtBeginningOfExpression ::
  Foldable t =>
  Has (ErrorEff.Error ParserError) sig m =>
  Has (State ParserState) sig m =>
  Has Empty sig m =>
  m a ->
  t TokenType ->
  m b
binaryOperatorAtBeginningOfExpression termParser opTypes = do
  op <- ParserState.match opTypes
  let error =
        ParserError (Just op) $
          "Binary operator "
            <> tokenLexeme op
            <> " found at the beginning of expression."
  synchronizeByConsuming termParser
  ErrorEff.throwError error

synchronizeByConsuming ::
  Has (ErrorEff.Catch ParserError) sig m =>
  m a ->
  m ()
synchronizeByConsuming termParser =
  ErrorEff.catchError @ParserError
    (void termParser)
    (const $ pure ())

consume ::
  Foldable t =>
  Has (State ParserState) sig m =>
  Has (ErrorEff.Throw ParserError) sig m =>
  t TokenType ->
  T.Text ->
  m Token
consume tkTypes errorMsg =
  ParserState.match tkTypes `Util.recoverFromEmptyWith` throwParserError errorMsg
