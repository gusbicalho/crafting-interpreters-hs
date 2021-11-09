{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module HSLox.TreeWalk.Interpreter (
  interpret,
  interpretNext,
  baseEnv,
) where

import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Carrier.State.Church qualified as State.Church
import Control.Effect.Error (Throw)
import Control.Effect.State qualified as State
import Control.Monad (when)
import Data.Foldable (foldl', for_)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import HSLox.AST qualified as AST
import HSLox.AST.Meta (WithMeta)
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.Cells.Effect qualified as Cells
import HSLox.NativeFns.Effect qualified as NativeFns
import HSLox.StaticAnalysis.Analyzer qualified as Analyzer
import HSLox.Token (Token (..))
import HSLox.Token qualified as Token
import HSLox.TreeWalk.RTError qualified as RTError
import HSLox.TreeWalk.RTReturn qualified as RTReturn
import HSLox.TreeWalk.RTState qualified as RTState
import HSLox.TreeWalk.Runtime (Runtime)
import HSLox.TreeWalk.Runtime qualified as Runtime
import HSLox.Util qualified as Util

baseEnv ::
  forall cell sig m.
  Has (Cells.Cells cell) sig m =>
  m (Runtime.RTState cell)
baseEnv = State.Church.execState RTState.newState $ do
  RTState.defineM @cell "clock" $
    Runtime.NativeDef
      0
      ( \_ _ ->
          Runtime.ValNum . fromIntegral <$> NativeFns.clock
      )
  RTState.defineM @cell "print" $
    Runtime.NativeDef
      1
      ( \_ args -> case args of
          arg :<| _ -> do
            NativeFns.printText (showValue arg)
            pure Runtime.ValNil
          _ -> pure Runtime.ValNil
      )

interpret ::
  forall cell sig m meta.
  Has (Cells.Cells cell) sig m =>
  Has NativeFns.NativeFns sig m =>
  Runtime.RuntimeMeta meta =>
  AST.Program meta ->
  m (Maybe Runtime.RTError)
interpret prog = do
  env <- baseEnv @cell
  (_, rtError) <- interpretNext env prog
  pure rtError
{-# INLINE interpret #-}

interpretNext ::
  forall cell sig m meta.
  Has (Cells.Cells cell) sig m =>
  Has NativeFns.NativeFns sig m =>
  Runtime.RuntimeMeta meta =>
  Runtime.RTState cell ->
  AST.Program meta ->
  m (Runtime.RTState cell, Maybe Runtime.RTError)
interpretNext env prog =
  pure prog
    & (interpretStmt @cell =<<)
    & RTReturn.runReturn @cell
    & Util.runErrorToEither @Runtime.RTError
    & fmap (Util.rightToMaybe . Util.swapEither)
    & Util.runStateToPair env
{-# INLINE interpretNext #-}

showValue :: Runtime.RTValue cell -> T.Text
showValue (Runtime.ValString s) = s
showValue (Runtime.ValBool True) = "true"
showValue (Runtime.ValBool False) = "false"
showValue Runtime.ValNil = "nil"
showValue (Runtime.ValFn (Runtime.LoxFn (AST.Function tk _ _ _) _ _)) =
  "<fn " <> tokenLexeme tk <> ">"
showValue (Runtime.ValClass (Runtime.LoxClass className _ _)) =
  "<class " <> className <> ">"
showValue (Runtime.ValInstance (Runtime.LoxInstance (Runtime.LoxClass className _ _) _)) =
  "<instance " <> className <> ">"
showValue (Runtime.ValNativeFn fn) = T.pack $ show fn
showValue (Runtime.ValNum d) = dropZeroDecimal doubleString
 where
  doubleString = T.pack $ show d
  dropZeroDecimal numStr
    | T.takeEnd 2 numStr == ".0" = T.dropEnd 2 numStr
    | otherwise = numStr

class
  StmtInterpreter
    (cell :: Type -> Type)
    (e :: Type)
    (m :: Type -> Type)
  where
  interpretStmt :: e -> m ()

instance (StmtInterpreter cell e m) => StmtInterpreter cell (WithMeta meta e) m where
  {-# INLINE interpretStmt #-}
  interpretStmt = interpretStmt @cell . AST.Meta.content

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  StmtInterpreter cell (AST.Program meta) m
  where
  interpretStmt (AST.Program stmts) = for_ stmts (interpretStmt @cell)

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  StmtInterpreter cell (AST.Stmt meta) m
  where
  {-# INLINE interpretStmt #-}
  interpretStmt (AST.ExprStmt expr) = interpretExpr @cell expr $> ()
  interpretStmt (AST.VarDeclarationStmt decl) = interpretStmt @cell decl
  interpretStmt (AST.FunDeclarationStmt decl) = interpretStmt @cell decl
  interpretStmt (AST.ClassDeclarationStmt decl) = interpretStmt @cell decl
  interpretStmt (AST.BlockStmt block) = interpretStmt @cell block
  interpretStmt (AST.IfStmt ifStmt) = interpretStmt @cell ifStmt
  interpretStmt (AST.WhileStmt whileStmt) = interpretStmt @cell whileStmt
  interpretStmt (AST.ReturnStmt return) = interpretStmt @cell return

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  StmtInterpreter cell (AST.VarDeclaration meta) m
  where
  interpretStmt (AST.VarDeclaration tk expr) = do
    val <- interpretExpr @cell expr
    RTState.defineM (tokenLexeme tk) val

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  StmtInterpreter cell (AST.FunDeclaration meta) m
  where
  interpretStmt (AST.FunDeclaration tk expr) = do
    let name = tokenLexeme tk
    val <- interpretExpr @cell expr
    RTState.defineM @cell name val

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  StmtInterpreter cell (AST.ClassDeclaration meta) m
  where
  interpretStmt (AST.ClassDeclaration tk superclassVar methodExprs) = do
    superclass <- traverse getSuperclass superclassVar
    RTState.defineM @cell (tokenLexeme tk) Runtime.ValNil
    classFrame <- case superclass of
      Nothing -> State.gets (RTState.localFrame @cell)
      Just super' -> Just <$> RTState.childFrameWithBinding "super" (Runtime.ValClass super')
    RTState.assignM @cell tk (buildClass superclass classFrame)
   where
    getSuperclass variable = do
      super <- interpretExpr @cell variable
      case super of
        Runtime.ValClass super -> pure super
        _ ->
          RTError.throwRT
            (AST.variableIdentifier . AST.Meta.content $ variable)
            "Superclass must be a class."
    buildClass super frame = Runtime.ValClass $ Runtime.LoxClass (tokenLexeme tk) super (methodTable frame methodExprs)
    methodTable frame methodExprs = foldl' (addMethod frame) Map.empty methodExprs
    isInit (AST.Function tk _ _ _) = tokenLexeme tk == "init"
    addMethod frame table (AST.Meta.content -> methodExpr) =
      Map.insert
        (tokenLexeme $ AST.functionMarker methodExpr)
        (Runtime.LoxFn methodExpr frame (isInit methodExpr))
        table

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  StmtInterpreter cell (AST.Block meta) m
  where
  interpretStmt (AST.Block stmts) =
    RTState.runInChildEnv @cell $ do
      for_ stmts (interpretStmt @cell)

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  StmtInterpreter cell (AST.If meta) m
  where
  interpretStmt (AST.If cond thenStmt elseStmt) = do
    cond <- interpretExpr @cell cond
    if isTruthy cond
      then interpretStmt @cell thenStmt
      else maybe (pure ()) (interpretStmt @cell) elseStmt

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  StmtInterpreter cell (AST.While meta) m
  where
  interpretStmt (AST.While cond body) =
    Util.whileM (isTruthy <$> interpretExpr @cell cond) $
      interpretStmt @cell body

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  StmtInterpreter cell (AST.Return meta) m
  where
  interpretStmt (AST.Return tk expr) = do
    case expr of
      Just expr -> do
        val <- interpretExpr @cell expr
        RTReturn.throwReturn tk val
      Nothing -> RTReturn.throwReturn @cell tk Runtime.ValNil

class
  ExprInterpreter
    (cell :: Type -> Type)
    (e :: Type)
    (m :: Type -> Type)
  where
  interpretExpr :: e -> m (Runtime.RTValue cell)

instance (ExprInterpreter cell e m) => ExprInterpreter cell (WithMeta meta e) m where
  {-# INLINE interpretExpr #-}
  interpretExpr = interpretExpr @cell . AST.Meta.content

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (AST.Expr meta) m
  where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.UnaryExpr t) = interpretExpr t
  interpretExpr (AST.LogicalExpr t) = interpretExpr t
  interpretExpr (AST.BinaryExpr t) = interpretExpr t
  interpretExpr (AST.TernaryExpr t) = interpretExpr t
  interpretExpr (AST.GroupingExpr t) = interpretExpr t
  interpretExpr (AST.LiteralExpr t) = interpretExpr t
  interpretExpr (AST.CallExpr t) = interpretExpr t
  interpretExpr (AST.GetPropertyExpr t) = interpretExpr t
  interpretExpr (AST.SetPropertyExpr t) = interpretExpr t
  interpretExpr (AST.ThisExpr t) = interpretExpr t
  interpretExpr (AST.SuperExpr t) = interpretExpr t
  interpretExpr (AST.FunctionExpr t) = interpretExpr t
  interpretExpr (AST.VariableExpr t) = interpretExpr t
  interpretExpr (AST.AssignmentExpr t) = interpretExpr t

instance
  {-# OVERLAPPING #-}
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (WithMeta meta AST.This) m
  where
  interpretExpr this = do
    let (AST.This tk) = AST.Meta.content this
    let resolverMeta = AST.Meta.getMetaItem @Analyzer.ResolverMeta this
    RTState.getBoundValueAtM tk (Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta)

instance
  {-# OVERLAPPING #-}
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (WithMeta meta AST.Variable) m
  where
  interpretExpr variable = do
    let (AST.Variable tk) = AST.Meta.content variable
    let resolverMeta = AST.Meta.getMetaItem @Analyzer.ResolverMeta variable
    RTState.getBoundValueAtM tk (Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta)

instance
  {-# OVERLAPPING #-}
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (WithMeta meta (AST.Assignment meta)) m
  where
  interpretExpr assignment = do
    let (AST.Assignment tk expr) = AST.Meta.content assignment
    val <- interpretExpr expr
    let resolverMeta = AST.Meta.getMetaItem @Analyzer.ResolverMeta assignment
    let distance = Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta
    RTState.assignAtM tk distance val
    pure val

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (AST.Ternary meta) m
  where
  interpretExpr (AST.Ternary left op1 middle op2 right) = do
    leftVal <- interpretExpr @cell left
    case (tokenType op1, tokenType op2) of
      (Token.QUESTION_MARK, Token.COLON) ->
        if isTruthy leftVal
          then interpretExpr middle
          else interpretExpr right
      _ ->
        RTError.throwRT op2 $
          "AST Error: Operator pair "
            <> tokenLexeme op1
            <> " and "
            <> tokenLexeme op2
            <> " not supported in ternary position"

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (AST.Logical meta) m
  where
  interpretExpr (AST.Logical left op right) = do
    leftVal <- interpretExpr left
    case tokenType op of
      Token.OR ->
        if isTruthy leftVal
          then pure leftVal
          else interpretExpr right
      Token.AND ->
        if not . isTruthy $ leftVal
          then pure leftVal
          else interpretExpr right
      _ ->
        RTError.throwRT op $
          "AST Error: Operator "
            <> tokenLexeme op
            <> " not supported in logical position"

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (AST.Binary meta) m
  where
  interpretExpr (AST.Binary left op right) = do
    leftVal <- interpretExpr left
    rightVal <- interpretExpr right
    case tokenType op of
      Token.COMMA -> pure rightVal
      Token.PLUS -> sumVals op leftVal rightVal
      Token.MINUS -> applyNumericOp op (-) leftVal rightVal
      Token.STAR -> applyNumericOp op (*) leftVal rightVal
      Token.SLASH -> applyNumericOp op (/) leftVal rightVal
      Token.GREATER -> applyComparisonOp op (>) leftVal rightVal
      Token.GREATER_EQUAL -> applyComparisonOp op (>=) leftVal rightVal
      Token.LESS -> applyComparisonOp op (<) leftVal rightVal
      Token.LESS_EQUAL -> applyComparisonOp op (<=) leftVal rightVal
      Token.EQUAL_EQUAL -> pure . Runtime.ValBool $ isEqual leftVal rightVal
      Token.BANG_EQUAL -> pure . Runtime.ValBool . not $ isEqual leftVal rightVal
      _ ->
        RTError.throwRT op $
          "AST Error: Operator "
            <> tokenLexeme op
            <> " not supported in binary position"
   where
    applyNumericOp opTk op v1 v2 = Runtime.ValNum . uncurry op <$> numericOperands opTk v1 v2
    applyComparisonOp opTk op v1 v2 = Runtime.ValBool . uncurry op <$> numericOperands opTk v1 v2
    sumVals _ (Runtime.ValNum d1) (Runtime.ValNum d2) = pure $ Runtime.ValNum (d1 + d2)
    sumVals _ (Runtime.ValString s1) (Runtime.ValString s2) = pure $ Runtime.ValString (s1 <> s2)
    sumVals opTk _ _ = RTError.throwRT opTk "Operands must be two numbers or two strings."

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (AST.Unary meta) m
  where
  interpretExpr (AST.Unary op expr) = do
    val <- interpretExpr @cell expr
    case tokenType op of
      Token.BANG -> pure . Runtime.ValBool . not . isTruthy $ val
      Token.MINUS -> Runtime.ValNum . negate <$> numericOperand op val
      _ ->
        RTError.throwRT op $
          "AST Error: Operator "
            <> tokenLexeme op
            <> " not supported in unary position"

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (AST.Grouping meta) m
  where
  interpretExpr (AST.Grouping expr) = interpretExpr expr

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (AST.Function meta) m
  where
  interpretExpr fn = do
    frame <- State.gets (RTState.localFrame @cell)
    pure . Runtime.ValFn $ Runtime.LoxFn fn frame False

instance Runtime cell sig m => ExprInterpreter cell AST.Literal m where
  interpretExpr (AST.LitString s) = pure $ Runtime.ValString s
  interpretExpr (AST.LitNum d) = pure $ Runtime.ValNum d
  interpretExpr (AST.LitBool b) = pure $ Runtime.ValBool b
  interpretExpr AST.LitNil = pure Runtime.ValNil

instance
  {-# OVERLAPPING #-}
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (WithMeta meta AST.Super) m
  where
  interpretExpr super = do
    let (AST.Super keyword propertyName) = AST.Meta.content super
    let resolverMeta = AST.Meta.getMetaItem @Analyzer.ResolverMeta super
    let distance = Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta
    superclass <- RTState.getBoundValueAtM @cell keyword distance
    superclass <- case superclass of
      Runtime.ValClass superclass -> pure superclass
      _ -> RTError.throwRT keyword $ "Invalid 'super' binding, expected a class, found " <> showValue superclass
    this <- RTState.getBoundValueAtM (Token "this" Token.THIS Nothing 0) (pred <$> distance)
    this <- case this of
      Runtime.ValInstance inst -> pure inst
      _ -> RTError.throwRT keyword $ "Invalid 'this' binding, expected an instance, found " <> showValue this
    let methodName = tokenLexeme propertyName
    case findMethod methodName superclass of
      Just method -> Runtime.ValFn <$> bindThis this method
      _ -> RTError.throwRT propertyName $ "Undefined method '" <> methodName <> "'."

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (AST.GetProperty meta) m
  where
  interpretExpr (AST.GetProperty objectExpr tk) = do
    object <- interpretExpr objectExpr
    case object of
      Runtime.ValInstance inst -> getProperty @cell inst tk
      _ -> RTError.throwRT tk "Only instances have properties."

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (AST.SetProperty meta) m
  where
  interpretExpr (AST.SetProperty objectExpr tk valueExpr) = do
    object <- interpretExpr objectExpr
    case object of
      Runtime.ValInstance inst -> do
        value <- interpretExpr valueExpr
        setProperty @cell inst tk value
      _ -> RTError.throwRT tk "Only instances have properties."

getProperty ::
  forall cell sig m.
  Runtime cell sig m =>
  Runtime.LoxInstance cell ->
  Token ->
  m (Runtime.RTValue cell)
getProperty inst@(Runtime.LoxInstance klass properties) propertyName = do
  let name = tokenLexeme propertyName
  props <- Cells.readCell properties
  case Map.lookup name props of
    Just val -> pure val
    Nothing -> do
      case findMethod name klass of
        Just method -> Runtime.ValFn <$> bindThis inst method
        Nothing -> RTError.throwRT propertyName $ "Undefined property '" <> name <> "'."

findMethod :: T.Text -> Runtime.LoxClass cell -> Maybe (Runtime.LoxFn cell)
findMethod name (Runtime.LoxClass _ superclass methodTable) =
  Map.lookup name methodTable <|> (findMethod name =<< superclass)

bindThis ::
  Runtime cell sig m =>
  Runtime.LoxInstance cell ->
  Runtime.LoxFn cell ->
  m (Runtime.LoxFn cell)
bindThis inst (Runtime.LoxFn function env isInit) = RTState.runInChildEnvOf env $ do
  RTState.defineM "this" (Runtime.ValInstance inst)
  Runtime.LoxFn function <$> State.gets RTState.localFrame <*> pure isInit

setProperty ::
  forall cell sig m.
  Runtime cell sig m =>
  Runtime.LoxInstance cell ->
  Token ->
  Runtime.RTValue cell ->
  m (Runtime.RTValue cell)
setProperty (Runtime.LoxInstance _klass properties) propertyName value = do
  let name = tokenLexeme propertyName
  Cells.updateCell (Map.insert name value) properties
  pure value

instance
  ( Runtime cell sig m
  , Runtime.RuntimeMeta meta
  ) =>
  ExprInterpreter cell (AST.Call meta) m
  where
  interpretExpr (AST.Call calleeExpr paren argExprs) = do
    callee <- interpretExpr @cell calleeExpr
    args <- traverse interpretExpr argExprs
    case callee of
      Runtime.ValFn fn -> call paren fn args
      Runtime.ValClass klass -> call paren klass args
      Runtime.ValNativeFn nativeFn -> call paren nativeFn args
      _ -> RTError.throwRT paren "Can only call functions and classes."

call ::
  forall cell e sig m.
  LoxCallable cell e m =>
  Has (Throw Runtime.RTError) sig m =>
  Token ->
  e ->
  Seq (Runtime.RTValue cell) ->
  m (Runtime.RTValue cell)
call paren callee args = do
  arity <- loxArity @cell callee
  let argCount = Seq.length args
  when (arity /= argCount) $
    RTError.throwRT paren $
      "Expected "
        <> T.pack (show arity)
        <> " arguments, but got "
        <> T.pack (show argCount)
        <> "."
  loxCall paren callee args

class LoxCallable cell e m where
  loxArity :: e -> m Int
  loxCall :: Token -> e -> Seq (Runtime.RTValue cell) -> m (Runtime.RTValue cell)

instance Runtime cell sig m => LoxCallable cell (Runtime.LoxFn cell) m where
  loxArity (Runtime.LoxFn (AST.Function _ _ params _) _ _) = pure (Seq.length params)
  loxCall _ fn@(Runtime.LoxFn (AST.Function _ fnRecId params body) env isInit) args = do
    returnVal <- RTReturn.catchReturn $
      RTState.runInChildEnvOf env $ do
        case fnRecId of
          Just fnName -> RTState.defineM (Token.tokenLexeme fnName) (Runtime.ValFn fn)
          Nothing -> pure ()
        for_ (Seq.zip params args) $ \(param, arg) -> do
          RTState.defineM (tokenLexeme param) arg
        interpretStmt @cell body
        pure Runtime.ValNil
    if isInit
      then RTState.runInChildEnvOf env $ do
        RTState.getBoundValueAtM (Token "this" Token.THIS Nothing 0) (Just 1)
      else pure returnVal

instance Runtime cell sig m => LoxCallable cell (Runtime.LoxClass cell) m where
  loxArity klass = case findMethod "init" klass of
    Nothing -> pure 0
    Just fn -> loxArity @cell fn
  loxCall tk klass args = do
    instanceState <- Cells.newCell @cell Map.empty
    let inst = Runtime.LoxInstance klass instanceState
    case findMethod "init" klass of
      Nothing -> pure ()
      Just init -> do
        boundInit <- bindThis inst init
        call tk boundInit args
        pure ()
    pure $ Runtime.ValInstance inst

instance Runtime cell sig m => LoxCallable cell Runtime.LoxNativeFn m where
  loxArity (Runtime.LoxNativeFn arity _) = pure arity
  loxCall tk (Runtime.LoxNativeFn _ impl) args = Runtime.runNativeFnImpl impl tk args

isTruthy :: Runtime.RTValue cell -> Bool
isTruthy (Runtime.ValBool b) = b
isTruthy Runtime.ValNil = False
isTruthy _ = True

isEqual :: Runtime.RTValue cell -> Runtime.RTValue cell -> Bool
isEqual (Runtime.ValString s1) (Runtime.ValString s2) = s1 == s2
isEqual (Runtime.ValNum d1) (Runtime.ValNum d2) = d1 == d2
isEqual (Runtime.ValBool b1) (Runtime.ValBool b2) = b1 == b2
isEqual Runtime.ValNil Runtime.ValNil = True
isEqual _ _ = False

numericOperand ::
  Has (Throw Runtime.RTError) sig m =>
  Token ->
  Runtime.RTValue cell ->
  m Double
numericOperand _ (Runtime.ValNum n) = pure n
numericOperand opTk _ = RTError.throwRT opTk "Operand must be a number."

numericOperands ::
  Has (Throw Runtime.RTError) sig m =>
  Token ->
  Runtime.RTValue cell ->
  Runtime.RTValue cell ->
  m (Double, Double)
numericOperands _ (Runtime.ValNum n1) (Runtime.ValNum n2) = pure (n1, n2)
numericOperands opTk _ _ = RTError.throwRT opTk "Operands must be numbers."
