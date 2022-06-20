{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
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
import Control.Monad (unless, when)
import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Functor.Identity qualified as Identity
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Traversable qualified as T
import HSLox.AST qualified as AST
import HSLox.AST.Meta (WithMeta)
import HSLox.AST.Meta qualified as AST.Meta
import HSLox.Cells.Effect qualified as Cells
import HSLox.NativeFns.Effect qualified as NativeFns
import HSLox.StaticAnalysis.Analyzer qualified as Analyzer
import HSLox.Token (Token (..))
import HSLox.Token qualified as Token
import HSLox.TreeWalk.BuildError (BuildError (..))
import HSLox.TreeWalk.BuildError qualified as BuildError
import HSLox.TreeWalk.RTError qualified as RTError
import HSLox.TreeWalk.RTReturn qualified as RTReturn
import HSLox.TreeWalk.RTState qualified as RTState
import HSLox.TreeWalk.Runtime (RTError (..), Runtime)
import HSLox.TreeWalk.Runtime qualified as Runtime
import HSLox.Util qualified as Util

type InterpreterMeta meta = AST.Meta.HasMetaItem Analyzer.ResolverMeta meta

baseEnv ::
  forall cell sig m.
  Has (Cells.Cells cell) sig m =>
  m (Runtime.RTState cell)
baseEnv = State.Church.execState RTState.newState do
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
  InterpreterMeta meta =>
  AST.Program meta ->
  m (Maybe Runtime.RTError)
{-# INLINE interpret #-}
interpret prog = do
  env <- baseEnv @cell
  (_, rtError) <- interpretNext env prog
  pure rtError

interpretNext ::
  forall cell sig m meta.
  Has (Cells.Cells cell) sig m =>
  Has NativeFns.NativeFns sig m =>
  InterpreterMeta meta =>
  Runtime.RTState cell ->
  AST.Program meta ->
  m (Runtime.RTState cell, Maybe Runtime.RTError)
{-# INLINE interpretNext #-}
interpretNext env prog =
  interpretStmt prog
    & Util.runErrorToEither
    & Identity.runIdentity
    & \case
      Left (BuildError msg tk) ->
        -- TODO propagate BuildError as separate thing
        pure (env, Just (RTError ("Build error: " <> msg) tk))
      Right stmt ->
        Runtime.runAction @cell stmt
          & RTReturn.runReturn @cell
          & Util.runErrorToEither @Runtime.RTError
          & fmap (Util.rightToMaybe . Util.swapEither)
          & Util.runStateToPair env

showValue :: Runtime.RTValue cell -> T.Text
showValue (Runtime.ValString s) = s
showValue (Runtime.ValBool True) = "true"
showValue (Runtime.ValBool False) = "false"
showValue Runtime.ValNil = "nil"
showValue (Runtime.ValFn Runtime.LoxFn{Runtime.loxFnIdentifier}) =
  "<fn " <> Maybe.fromMaybe "[anonymous]" loxFnIdentifier <> ">"
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

class StmtInterpreter (e :: Type) where
  interpretStmt ::
    forall cell mBuild sigBuild.
    ( Monad mBuild
    , Has (Throw BuildError) sigBuild mBuild
    ) =>
    e ->
    mBuild (Runtime.RuntimeAction cell ())

instance (StmtInterpreter e) => StmtInterpreter (WithMeta meta e) where
  {-# INLINE interpretStmt #-}
  interpretStmt = interpretStmt . AST.Meta.content

instance
  (InterpreterMeta meta) =>
  StmtInterpreter (AST.Program meta)
  where
  {-# INLINE interpretStmt #-}
  interpretStmt (AST.Program stmts) = do
    actions <- T.for stmts interpretStmt
    Runtime.inRuntime \(_ :: p cell) -> do
      (F.traverse_ (Runtime.runAction @cell) actions)

instance
  (InterpreterMeta meta) =>
  StmtInterpreter (AST.Stmt meta)
  where
  {-# INLINE interpretStmt #-}
  interpretStmt (AST.ExprStmt expr) = do
    runExpr <- interpretExpr expr
    Runtime.inRuntime \(_ :: p cell) -> do
      _ <- Runtime.runAction @cell runExpr
      pure ()
  interpretStmt (AST.VarDeclarationStmt decl) = interpretStmt decl
  interpretStmt (AST.FunDeclarationStmt decl) = interpretStmt decl
  interpretStmt (AST.ClassDeclarationStmt decl) = interpretStmt decl
  interpretStmt (AST.BlockStmt block) = interpretStmt block
  interpretStmt (AST.IfStmt ifStmt) = interpretStmt ifStmt
  interpretStmt (AST.WhileStmt whileStmt) = interpretStmt whileStmt
  interpretStmt (AST.ReturnStmt return) = interpretStmt return

instance
  (InterpreterMeta meta) =>
  StmtInterpreter (AST.VarDeclaration meta)
  where
  {-# INLINE interpretStmt #-}
  interpretStmt (AST.VarDeclaration tk expr) = do
    let varName = tokenLexeme tk
    initialize <- interpretExpr expr
    Runtime.inRuntime \(_ :: p cell) -> do
      val <- Runtime.runAction @cell initialize
      RTState.defineM varName val

instance
  (InterpreterMeta meta) =>
  StmtInterpreter (AST.FunDeclaration meta)
  where
  {-# INLINE interpretStmt #-}
  interpretStmt (AST.FunDeclaration tk expr) = do
    let fnName = tokenLexeme tk
    initialize <- interpretExpr expr
    Runtime.inRuntime \(_ :: p cell) -> do
      val <- Runtime.runAction @cell initialize
      RTState.defineM fnName val

instance (InterpreterMeta meta) => StmtInterpreter (AST.ClassDeclaration meta) where
  {-# INLINE interpretStmt #-}
  interpretStmt = \(AST.ClassDeclaration classTk mbSuperclassVar methodExprs) -> do
    methodTable <- F.foldlM addMethod Map.empty methodExprs
    let className = tokenLexeme classTk
    case mbSuperclassVar of
      Nothing -> Runtime.inRuntime \(_ :: p cell) -> do
        RTState.defineM @cell className Runtime.ValNil
        classFrame <- State.gets (RTState.localFrame @cell)
        RTState.assignM classTk (buildClass className Nothing classFrame methodTable)
      Just superclassVar -> do
        resolveSuperclass <- interpretExpr superclassVar
        Runtime.inRuntime \(_ :: p cell) -> do
          superclass <- checkSuperIsAClass superclassVar =<< Runtime.runAction resolveSuperclass
          RTState.defineM @cell className Runtime.ValNil
          classFrame <- Just <$> RTState.childFrameWithBinding "super" (Runtime.ValClass superclass)
          RTState.assignM classTk (buildClass className (Just superclass) classFrame methodTable)
   where
    checkSuperIsAClass variable super = do
      case super of
        Runtime.ValClass super -> pure super
        _ ->
          RTError.throwRT
            (AST.variableIdentifier . AST.Meta.content $ variable)
            "Superclass must be a class."
    buildClass className super frame methodTable =
      Runtime.ValClass $ Runtime.LoxClass className super (fmap ($ frame) methodTable)
    addMethod table (AST.Meta.content -> methodExpr) = do
      makeMethod <- compileFn methodExpr (isInit methodExpr)
      pure $
        Map.insert
          (tokenLexeme $ AST.functionMarker methodExpr)
          makeMethod
          table
    isInit (AST.Function tk _ _ _) = tokenLexeme tk == "init"

instance (InterpreterMeta meta) => StmtInterpreter (AST.Block meta) where
  {-# INLINE interpretStmt #-}
  interpretStmt (AST.Block stmts) = do
    runStmts <- T.for stmts (interpretStmt)
    Runtime.inRuntime \(_ :: p cell) -> do
      RTState.runInChildEnv @cell do
        F.traverse_ (Runtime.runAction @cell) runStmts

instance (InterpreterMeta meta) => StmtInterpreter (AST.If meta) where
  {-# INLINE interpretStmt #-}
  interpretStmt (AST.If cond thenStmt elseStmt) = do
    runCond <- interpretExpr cond
    runThen <- interpretStmt thenStmt
    runElse <- maybe (Runtime.inRuntime \_ -> pure ()) (interpretStmt) elseStmt
    Runtime.inRuntime \(_ :: p cell) -> do
      cond <- Runtime.runAction @cell runCond
      if isTruthy cond
        then Runtime.runAction @cell runThen
        else Runtime.runAction @cell runElse

instance (InterpreterMeta meta) => StmtInterpreter (AST.While meta) where
  {-# INLINE interpretStmt #-}
  interpretStmt (AST.While cond body) = do
    runCond <- interpretExpr cond
    runBody <- interpretStmt body
    Runtime.inRuntime \(_ :: p cell) -> do
      Util.whileM (isTruthy <$> Runtime.runAction @cell runCond) do
        Runtime.runAction @cell runBody

instance (InterpreterMeta meta) => StmtInterpreter (AST.Return meta) where
  {-# INLINE interpretStmt #-}
  interpretStmt (AST.Return tk expr) = do
    case expr of
      Just expr -> do
        runVal <- interpretExpr expr
        Runtime.inRuntime \(_ :: p cell) -> do
          val <- Runtime.runAction @cell runVal
          RTReturn.throwReturn tk val
      Nothing -> do
        Runtime.inRuntime \(_ :: p cell) -> do
          RTReturn.throwReturn @cell tk Runtime.ValNil

class ExprInterpreter (e :: Type) where
  interpretExpr ::
    forall cell mBuild sigBuild.
    ( Monad mBuild
    , Has (Throw BuildError) sigBuild mBuild
    ) =>
    e ->
    mBuild (Runtime.RuntimeAction cell (Runtime.RTValue cell))

instance
  ExprInterpreter e =>
  ExprInterpreter (WithMeta meta e)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr = interpretExpr . AST.Meta.content

instance
  (InterpreterMeta meta) =>
  ExprInterpreter (AST.Expr meta)
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
  (InterpreterMeta meta) =>
  ExprInterpreter (WithMeta meta AST.This)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr this = do
    let (AST.This tk) = AST.Meta.content this
    let resolverMeta = AST.Meta.getMetaItem @Analyzer.ResolverMeta this
    let scopeDistance = Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta
    Runtime.inRuntime \(_ :: p cell) -> do
      RTState.getBoundValueAtM tk scopeDistance

instance
  {-# OVERLAPPING #-}
  (InterpreterMeta meta) =>
  ExprInterpreter (WithMeta meta AST.Variable)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr variable = do
    let (AST.Variable tk) = AST.Meta.content variable
    let resolverMeta = AST.Meta.getMetaItem @Analyzer.ResolverMeta variable
    let scopeDistance = Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta
    Runtime.inRuntime \(_ :: p cell) -> do
      RTState.getBoundValueAtM tk scopeDistance

instance
  {-# OVERLAPPING #-}
  (InterpreterMeta meta) =>
  ExprInterpreter (WithMeta meta (AST.Assignment meta))
  where
  {-# INLINE interpretExpr #-}
  interpretExpr assignment = do
    let (AST.Assignment tk expr) = AST.Meta.content assignment
    let resolverMeta = AST.Meta.getMetaItem @Analyzer.ResolverMeta assignment
    let distance = Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta
    runVal <- interpretExpr expr
    Runtime.inRuntime \(_ :: p cell) -> do
      val <- Runtime.runAction runVal
      RTState.assignAtM tk distance val
      pure val

instance
  (InterpreterMeta meta) =>
  ExprInterpreter (AST.Ternary meta)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.Ternary left op1 middle op2 right) = do
    runLeft <- interpretExpr left
    runMiddle <- interpretExpr middle
    runRight <- interpretExpr right
    unless (tokenType op1 == Token.QUESTION_MARK && tokenType op2 == Token.COLON) do
      BuildError.throwBuildError op2 $
        "AST Error: Operator pair "
          <> tokenLexeme op1
          <> " and "
          <> tokenLexeme op2
          <> " not supported in ternary position"
    Runtime.inRuntime \(_ :: p cell) -> do
      leftVal <- Runtime.runAction @cell runLeft
      if isTruthy leftVal
        then Runtime.runAction runMiddle
        else Runtime.runAction runRight

instance
  (InterpreterMeta meta) =>
  ExprInterpreter (AST.Logical meta)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.Logical left op right) = do
    runLeft <- interpretExpr left
    runRight <- interpretExpr right
    shouldStopOnLeftVal <- case tokenType op of
      Token.OR -> pure isTruthy
      Token.AND -> pure (not . isTruthy)
      _ ->
        BuildError.throwBuildError op $
          "AST Error: Operator "
            <> tokenLexeme op
            <> " not supported in logical position"
    Runtime.inRuntime \(_ :: p cell) -> do
      leftVal <- Runtime.runAction runLeft
      if shouldStopOnLeftVal leftVal
        then pure leftVal
        else Runtime.runAction runRight

instance
  ( InterpreterMeta meta
  ) =>
  ExprInterpreter (AST.Binary meta)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.Binary left op right) = do
    runLeft <- interpretExpr left
    runRight <- interpretExpr right
    runOp <- case tokenType op of
      Token.COMMA -> pure \_l r -> Runtime.RuntimeAction (pure r)
      Token.PLUS -> pure $ sumVals op
      Token.MINUS -> pure $ applyNumericOp op (-)
      Token.STAR -> pure $ applyNumericOp op (*)
      Token.SLASH -> pure $ applyNumericOp op (/)
      Token.GREATER -> pure $ applyComparisonOp op (>)
      Token.GREATER_EQUAL -> pure $ applyComparisonOp op (>=)
      Token.LESS -> pure $ applyComparisonOp op (<)
      Token.LESS_EQUAL -> pure $ applyComparisonOp op (<=)
      Token.EQUAL_EQUAL -> pure \l r -> Runtime.RuntimeAction $ pure . Runtime.ValBool $ isEqual l r
      Token.BANG_EQUAL -> pure \l r -> Runtime.RuntimeAction $ pure . Runtime.ValBool . not $ isEqual l r
      _ ->
        BuildError.throwBuildError op $
          "AST Error: Operator "
            <> tokenLexeme op
            <> " not supported in binary position"
    Runtime.inRuntime \(_ :: p cell) -> do
      left <- Runtime.runAction runLeft
      right <- Runtime.runAction runRight
      Runtime.runAction @cell (runOp left right)
   where
    applyNumericOp opTk op v1 v2 = Runtime.RuntimeAction do
      Runtime.ValNum . uncurry op <$> numericOperands opTk v1 v2
    applyComparisonOp opTk op v1 v2 = Runtime.RuntimeAction do
      Runtime.ValBool . uncurry op <$> numericOperands opTk v1 v2
    sumVals _ (Runtime.ValNum d1) (Runtime.ValNum d2) = Runtime.RuntimeAction do
      pure $ Runtime.ValNum (d1 + d2)
    sumVals _ (Runtime.ValString s1) (Runtime.ValString s2) = Runtime.RuntimeAction do
      pure $ Runtime.ValString (s1 <> s2)
    sumVals opTk _ _ = Runtime.RuntimeAction do
      RTError.throwRT opTk "Operands must be two numbers or two strings."

instance
  (InterpreterMeta meta) =>
  ExprInterpreter (AST.Unary meta)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.Unary op expr) = do
    runVal <- interpretExpr expr
    runOp <- case tokenType op of
      Token.BANG -> pure \val -> Runtime.RuntimeAction do
        pure . Runtime.ValBool . not . isTruthy $ val
      Token.MINUS -> pure \val -> Runtime.RuntimeAction do
        number <- numericOperand op val
        pure . Runtime.ValNum . negate $ number
      _ ->
        BuildError.throwBuildError op $
          "AST Error: Operator "
            <> tokenLexeme op
            <> " not supported in unary position"
    Runtime.inRuntime \(_ :: p cell) -> do
      val <- Runtime.runAction @cell runVal
      Runtime.runAction @cell (runOp val)

instance
  ( InterpreterMeta meta
  ) =>
  ExprInterpreter (AST.Grouping meta)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.Grouping expr) = interpretExpr expr

instance
  ( InterpreterMeta meta
  ) =>
  ExprInterpreter (AST.Function meta)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr fn = do
    makeFn <- compileFn fn False
    Runtime.inRuntime \(_ :: p cell) -> do
      frame <- State.gets RTState.localFrame
      pure . Runtime.ValFn $ makeFn frame

compileFn ::
  forall cell meta mBuild sigBuild.
  ( InterpreterMeta meta
  , Has (Throw BuildError) sigBuild mBuild
  ) =>
  AST.Function meta ->
  Bool ->
  mBuild
    ( Maybe (Runtime.RTFrame cell) ->
      Runtime.LoxFn cell
    )
{-# INLINE compileFn #-}
compileFn fnExpr@(AST.Function _ fnRecTk params _) isInit = do
  let fnRecId = Token.tokenLexeme <$> fnRecTk
  let paramIds = Token.tokenLexeme <$> params
  bodyAction <- interpretStmt (AST.functionBody fnExpr)
  let fnAction frame self args =
        Runtime.RuntimeAction @cell do
          RTReturn.catchReturn do
            RTState.runInChildEnvOf frame do
              case fnRecId of
                Just fnName -> RTState.defineM fnName self
                Nothing -> pure ()
              F.for_ (Seq.zip paramIds args) $ \(param, arg) -> do
                RTState.defineM param arg
              Runtime.runAction @cell bodyAction
              pure Runtime.ValNil
  pure \frame ->
    Runtime.LoxFn
      { Runtime.loxClosedEnv = frame
      , Runtime.loxFnIdentifier = fnRecId
      , Runtime.loxFnIsInitializer = isInit
      , Runtime.loxFnArity = Seq.length params
      , Runtime.loxFnAction = fnAction
      }

instance ExprInterpreter AST.Literal where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.LitString s) = Runtime.inRuntime \(_ :: p cell) -> do
    pure $ Runtime.ValString s
  interpretExpr (AST.LitNum d) = Runtime.inRuntime \(_ :: p cell) -> do
    pure $ Runtime.ValNum d
  interpretExpr (AST.LitBool b) = Runtime.inRuntime \(_ :: p cell) -> do
    pure $ Runtime.ValBool b
  interpretExpr AST.LitNil = Runtime.inRuntime \(_ :: p cell) -> do
    pure $ Runtime.ValNil

instance
  {-# OVERLAPPING #-}
  (InterpreterMeta meta) =>
  ExprInterpreter (WithMeta meta AST.Super)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr super = do
    let (AST.Super keyword propertyName) = AST.Meta.content super
    let resolverMeta = AST.Meta.getMetaItem @Analyzer.ResolverMeta super
    let distance = Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta
    let methodName = tokenLexeme propertyName
    Runtime.inRuntime \(_ :: p cell) -> do
      superclass <- RTState.getBoundValueAtM keyword distance
      superclass <- case superclass of
        Runtime.ValClass superclass -> pure superclass
        _ -> RTError.throwRT keyword $ "Invalid 'super' binding, expected a class, found " <> showValue superclass
      this <- RTState.getBoundValueAtM (Token "this" Token.THIS Nothing 0) (pred <$> distance)
      this <- case this of
        Runtime.ValInstance inst -> pure inst
        _ -> RTError.throwRT keyword $ "Invalid 'this' binding, expected an instance, found " <> showValue this
      case findMethod methodName superclass of
        Just method -> Runtime.ValFn <$> bindThis this method
        _ -> RTError.throwRT propertyName $ "Undefined method '" <> methodName <> "'."

instance
  (InterpreterMeta meta) =>
  ExprInterpreter (AST.GetProperty meta)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.GetProperty objectExpr tk) = do
    runObject <- interpretExpr objectExpr
    Runtime.inRuntime \(_ :: p cell) -> do
      object <- Runtime.runAction runObject
      case object of
        Runtime.ValInstance inst -> getProperty inst tk
        _ -> RTError.throwRT tk "Only instances have properties."

instance
  (InterpreterMeta meta) =>
  ExprInterpreter (AST.SetProperty meta)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.SetProperty objectExpr tk valueExpr) = do
    runObject <- interpretExpr objectExpr
    runValue <- interpretExpr valueExpr
    Runtime.inRuntime \(_ :: p cell) -> do
      object <- Runtime.runAction runObject
      case object of
        Runtime.ValInstance inst -> do
          value <- Runtime.runAction runValue
          setProperty inst tk value
        _ -> RTError.throwRT tk "Only instances have properties."

getProperty ::
  forall cell sig m.
  Runtime cell sig m =>
  Runtime.LoxInstance cell ->
  Token ->
  m (Runtime.RTValue cell)
{-# INLINE getProperty #-}
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
{-# INLINE findMethod #-}
findMethod name (Runtime.LoxClass _ superclass methodTable) =
  Map.lookup name methodTable <|> (findMethod name =<< superclass)

bindThis ::
  Runtime cell sig m =>
  Runtime.LoxInstance cell ->
  Runtime.LoxFn cell ->
  m (Runtime.LoxFn cell)
{-# INLINE bindThis #-}
bindThis inst loxFn@Runtime.LoxFn{Runtime.loxClosedEnv = env} = RTState.runInChildEnvOf env do
  RTState.defineM "this" (Runtime.ValInstance inst)
  frame <- State.gets RTState.localFrame
  pure $ loxFn{Runtime.loxClosedEnv = frame}

setProperty ::
  forall cell sig m.
  Runtime cell sig m =>
  Runtime.LoxInstance cell ->
  Token ->
  Runtime.RTValue cell ->
  m (Runtime.RTValue cell)
{-# INLINE setProperty #-}
setProperty (Runtime.LoxInstance _klass properties) propertyName value = do
  let name = tokenLexeme propertyName
  Cells.updateCell (Map.insert name value) properties
  pure value

instance
  (InterpreterMeta meta) =>
  ExprInterpreter (AST.Call meta)
  where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.Call calleeExpr paren argExprs) = do
    runCallee <- interpretExpr calleeExpr
    runArgs <- traverse (interpretExpr) argExprs
    Runtime.inRuntime \(_ :: p cell) -> do
      callee <- Runtime.runAction @cell runCallee
      args <- T.traverse Runtime.runAction runArgs
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
{-# INLINE call #-}
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
  {-# INLINE loxArity #-}
  loxArity Runtime.LoxFn{Runtime.loxFnArity} = pure loxFnArity
  {-# INLINE loxCall #-}
  loxCall
    _
    fn@Runtime.LoxFn
      { Runtime.loxClosedEnv = env
      , Runtime.loxFnIsInitializer = isInit
      , Runtime.loxFnAction = bodyAction
      }
    args = do
      returnVal <- Runtime.runAction (bodyAction env (Runtime.ValFn fn) args)
      if isInit
        then RTState.runInChildEnvOf env do
          RTState.getBoundValueAtM (Token "this" Token.THIS Nothing 0) (Just 1)
        else pure returnVal

instance Runtime cell sig m => LoxCallable cell (Runtime.LoxClass cell) m where
  {-# INLINE loxArity #-}
  loxArity klass = case findMethod "init" klass of
    Nothing -> pure 0
    Just fn -> loxArity @cell fn
  {-# INLINE loxCall #-}
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
  {-# INLINE loxArity #-}
  loxArity (Runtime.LoxNativeFn arity _) = pure arity
  {-# INLINE loxCall #-}
  loxCall tk (Runtime.LoxNativeFn _ impl) args = Runtime.runNativeFnImpl impl tk args

{-# INLINE isTruthy #-}
isTruthy :: Runtime.RTValue cell -> Bool
isTruthy (Runtime.ValBool b) = b
isTruthy Runtime.ValNil = False
isTruthy _ = True

{-# INLINE isEqual #-}
isEqual :: Runtime.RTValue cell -> Runtime.RTValue cell -> Bool
isEqual (Runtime.ValString s1) (Runtime.ValString s2) = s1 == s2
isEqual (Runtime.ValNum d1) (Runtime.ValNum d2) = d1 == d2
isEqual (Runtime.ValBool b1) (Runtime.ValBool b2) = b1 == b2
isEqual Runtime.ValNil Runtime.ValNil = True
isEqual _ _ = False

{-# INLINE numericOperand #-}
numericOperand ::
  Has (Throw Runtime.RTError) sig m =>
  Token ->
  Runtime.RTValue cell ->
  m Double
numericOperand _ (Runtime.ValNum n) = pure n
numericOperand opTk _ = RTError.throwRT opTk "Operand must be a number."

{-# INLINE numericOperands #-}
numericOperands ::
  Has (Throw Runtime.RTError) sig m =>
  Token ->
  Runtime.RTValue cell ->
  Runtime.RTValue cell ->
  m (Double, Double)
numericOperands _ (Runtime.ValNum n1) (Runtime.ValNum n2) = pure (n1, n2)
numericOperands opTk _ _ = RTError.throwRT opTk "Operands must be numbers."
