{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}
module HSLox.TreeWalk.Interpreter
  ( interpret
  , interpretNext
  , baseEnv
  ) where

import Control.Carrier.Error.Church
import Control.Carrier.State.Church
import Control.Monad (when)
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified HSLox.AST as AST
import qualified HSLox.AST.Meta as AST.Meta
import qualified HSLox.Cells.Effect as Cells
import qualified HSLox.NativeFns.Effect as NativeFns
import qualified HSLox.StaticAnalysis.Analyzer as Analyzer
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token
import qualified HSLox.TreeWalk.RTState as RTState
import qualified HSLox.TreeWalk.RTError as RTError
import qualified HSLox.TreeWalk.RTReturn as RTReturn
import HSLox.TreeWalk.Runtime
import qualified HSLox.Util as Util

baseEnv :: forall cell sig m
         . Has (Cells.Cells cell) sig m
        => m (RTState cell)
baseEnv = execState RTState.newState $ do
    RTState.defineM @cell "clock" $ NativeDef 0 (\_ _ ->
      ValNum . fromIntegral <$> NativeFns.clock)
    RTState.defineM @cell "print" $ NativeDef 1 (\_ args -> case args of
      arg :<| _ -> do
        NativeFns.printText (showValue arg)
        pure ValNil
      _ -> pure ValNil)

interpret :: forall cell sig m f
           . Has (Cells.Cells cell) sig m
          => Has NativeFns.NativeFns sig m
          => Traversable f
          => AST.Meta.AsIdentity f
          => AST.Meta.HasMeta Analyzer.ResolverMeta f
          => AST.Program f -> m (Maybe RTError)
interpret prog = do
  env <- baseEnv @cell
  (_, rtError) <- interpretNext env prog
  pure rtError
{-# INLINE interpret #-}

interpretNext :: forall cell sig m f
               . Has (Cells.Cells cell) sig m
              => Has NativeFns.NativeFns sig m
              => Traversable f
              => AST.Meta.AsIdentity f
              => AST.Meta.HasMeta Analyzer.ResolverMeta f
              => RTState cell
              -> AST.Program f
              -> m (RTState cell, Maybe RTError)
interpretNext env prog = prog & asRuntimeAST
                              & (interpretStmt @cell =<<)
                              & RTReturn.runReturn @cell
                              & Util.runErrorToEither @RTError
                              & fmap (Util.rightToMaybe . Util.swapEither)
                              & Util.runStateToPair env
{-# INLINE interpretNext #-}

showValue :: RTValue cell -> T.Text
showValue (ValString s) = s
showValue (ValBool True) = "true"
showValue (ValBool False) = "false"
showValue ValNil = "nil"
showValue (ValFn (LoxFn (AST.Function tk _ _) _ _)) = "<fn " <> tokenLexeme tk <> ">"
showValue (ValClass (LoxClass className _)) = "<class " <> className <> ">"
showValue (ValInstance (LoxInstance (LoxClass className _) _)) = "<instance " <> className <> ">"
showValue (ValNativeFn fn) = T.pack $ show fn
showValue (ValNum d) = dropZeroDecimal doubleString
  where
    doubleString = T.pack $ show d
    dropZeroDecimal numStr
      | T.takeEnd 2 numStr == ".0" = T.dropEnd 2 numStr
      | otherwise                  = numStr

class StmtInterpreter (cell :: Type -> Type)
                      (e :: Type)
                      (m :: Type -> Type) where
  interpretStmt :: e -> m ()

instance (StmtInterpreter cell e m, AST.Meta.AsIdentity f) => StmtInterpreter cell (f e) m where
  {-# INLINE interpretStmt #-}
  interpretStmt = interpretStmt @cell
                . AST.Meta.runIdentity
                . AST.Meta.asIdentity

instance ( Applicative m
         , StmtInterpreter cell (AST.Stmt f) m
         ) => StmtInterpreter cell (AST.Program f) m where
  interpretStmt (AST.Program stmts) = for_ stmts (interpretStmt @cell)

instance ( Applicative m
         , ExprInterpreter cell (f (AST.Expr f)) m
         , StmtInterpreter cell (f (AST.VarDeclaration f)) m
         , StmtInterpreter cell (f (AST.FunDeclaration f)) m
         , StmtInterpreter cell (f (AST.ClassDeclaration f)) m
         , StmtInterpreter cell (f (AST.Block f)) m
         , StmtInterpreter cell (f (AST.If f)) m
         , StmtInterpreter cell (f (AST.While f)) m
         , StmtInterpreter cell (f (AST.Return f)) m
         ) => StmtInterpreter cell (AST.Stmt f) m where
  {-# INLINE interpretStmt #-}
  interpretStmt (AST.ExprStmt expr) = interpretExpr @cell expr $> ()
  interpretStmt (AST.VarDeclarationStmt decl) = interpretStmt @cell decl
  interpretStmt (AST.FunDeclarationStmt decl) = interpretStmt @cell decl
  interpretStmt (AST.ClassDeclarationStmt decl) = interpretStmt @cell decl
  interpretStmt (AST.BlockStmt block) = interpretStmt @cell block
  interpretStmt (AST.IfStmt ifStmt) = interpretStmt @cell ifStmt
  interpretStmt (AST.WhileStmt whileStmt) = interpretStmt @cell whileStmt
  interpretStmt (AST.ReturnStmt return) = interpretStmt @cell return

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         ) => StmtInterpreter cell (AST.VarDeclaration f) m where
  interpretStmt (AST.VarDeclaration tk expr) = do
    val <- interpretExpr @cell expr
    RTState.defineM (tokenLexeme tk) val

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Function f) m
         ) => StmtInterpreter cell (AST.FunDeclaration f) m where
  interpretStmt (AST.FunDeclaration tk expr) = do
    let name = tokenLexeme tk
    RTState.defineM @cell name ValNil
    val <- interpretExpr @cell expr
    RTState.defineM @cell name val

instance Runtime cell sig m => StmtInterpreter cell (AST.ClassDeclaration RuntimeAST) m where
  interpretStmt (AST.ClassDeclaration tk methodExprs) = do
      let name = tokenLexeme tk
      RTState.defineM @cell name ValNil
      frame <- gets (RTState.localFrame @cell)
      let klass = ValClass $ LoxClass (tokenLexeme tk) (methodTable frame methodExprs)
      RTState.defineM @cell name klass
    where
      methodTable frame methodExprs = foldl' (addMethod frame) Map.empty methodExprs
      isInit (AST.Function tk _ _) = tokenLexeme tk == "init"
      addMethod frame table (AST.Meta.content -> methodExpr) =
        Map.insert (tokenLexeme $ AST.functionToken methodExpr)
                   (LoxFn methodExpr frame (isInit methodExpr))
                   table

instance ( Runtime cell sig m
         , StmtInterpreter cell (AST.Stmt f) m
         ) => StmtInterpreter cell (AST.Block f) m where
  interpretStmt (AST.Block stmts) =
    RTState.runInChildEnv @cell $ do
      for_ stmts (interpretStmt @cell)

instance ( Monad m
         , StmtInterpreter cell (AST.Stmt f) m
         , ExprInterpreter cell (AST.Expr f) m
         ) => StmtInterpreter cell (AST.If f) m where
  interpretStmt (AST.If cond thenStmt elseStmt) = do
    cond <- interpretExpr @cell cond
    if isTruthy cond
    then interpretStmt @cell thenStmt
    else maybe (pure ()) (interpretStmt @cell) elseStmt

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         , StmtInterpreter cell (AST.Stmt f) m
         ) => StmtInterpreter cell (AST.While f) m where
  interpretStmt (AST.While cond body) =
    Util.whileM (isTruthy <$> interpretExpr @cell cond) $
      interpretStmt @cell body

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         ) => StmtInterpreter cell (AST.Return f) m where
  interpretStmt (AST.Return tk expr) = do
    case expr of
      Just expr -> do
        val <- interpretExpr @cell expr
        RTReturn.throwReturn tk val
      Nothing -> RTReturn.throwReturn @cell tk ValNil

class ExprInterpreter (cell :: Type -> Type)
                      (e :: Type)
                      (m :: Type -> Type) where
  interpretExpr :: e -> m (RTValue cell)

instance (ExprInterpreter cell e m, AST.Meta.AsIdentity f) => ExprInterpreter cell (f e) m where
  {-# INLINE interpretExpr #-}
  interpretExpr = interpretExpr @cell
                . AST.Meta.runIdentity
                . AST.Meta.asIdentity

instance Runtime cell sig m => ExprInterpreter cell (AST.Expr RuntimeAST) m where
  {-# INLINE interpretExpr #-}
  interpretExpr (AST.UnaryExpr t) = interpretExpr t
  interpretExpr (AST.LogicalExpr t) = interpretExpr t
  interpretExpr (AST.BinaryExpr t) = interpretExpr t
  interpretExpr (AST.TernaryExpr t) = interpretExpr t
  interpretExpr (AST.GroupingExpr t) = interpretExpr t
  interpretExpr (AST.LiteralExpr t) = interpretExpr t
  interpretExpr (AST.CallExpr t) = interpretExpr t
  interpretExpr (AST.GetExpr t) = interpretExpr t
  interpretExpr (AST.SetPropertyExpr t) = interpretExpr t
  interpretExpr (AST.ThisExpr t) = interpretExpr t
  interpretExpr (AST.FunctionExpr t) = interpretExpr t
  interpretExpr (AST.VariableExpr t) = interpretExpr t
  interpretExpr (AST.AssignmentExpr t) = interpretExpr t

instance {-# OVERLAPPING #-} Runtime cell sig m
         => ExprInterpreter cell (RuntimeAST AST.This) m where
  interpretExpr this = do
    let (AST.This tk) = AST.Meta.content this
    let resolverMeta = AST.Meta.meta @Analyzer.ResolverMeta this
    RTState.getBoundValueAtM tk (Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta)

instance {-# OVERLAPPING #-} Runtime cell sig m
         => ExprInterpreter cell (RuntimeAST AST.Variable) m where
  interpretExpr variable = do
    let (AST.Variable tk) = AST.Meta.content variable
    let resolverMeta = AST.Meta.meta @Analyzer.ResolverMeta variable
    RTState.getBoundValueAtM tk (Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta)

instance {-# OVERLAPPING #-} Runtime cell sig m
         => ExprInterpreter cell (RuntimeAST (AST.Assignment RuntimeAST)) m where
  interpretExpr assignment = do
    let (AST.Assignment tk expr) = AST.Meta.content assignment
    val <- interpretExpr expr
    let resolverMeta = AST.Meta.meta @Analyzer.ResolverMeta assignment
    let distance = Analyzer.resolverMetaLocalVariableScopeDistance resolverMeta
    RTState.assignAtM tk distance val
    pure val

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         ) => ExprInterpreter cell (AST.Ternary f) m where
  interpretExpr (AST.Ternary left op1 middle op2 right) = do
    leftVal <- interpretExpr @cell left
    case (tokenType op1, tokenType op2) of
      (Token.QUESTION_MARK, Token.COLON) ->
        if isTruthy leftVal
        then interpretExpr middle
        else interpretExpr right
      _ -> RTError.throwRT op2 $ "AST Error: Operator pair "
                              <> tokenLexeme op1
                              <> " and "
                              <> tokenLexeme op2
                              <> " not supported in ternary position"

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         ) => ExprInterpreter cell (AST.Logical f) m where
  interpretExpr (AST.Logical left op right) = do
    leftVal <- interpretExpr left
    case tokenType op of
      Token.OR -> if isTruthy leftVal
                  then pure leftVal
                  else interpretExpr right
      Token.AND -> if not . isTruthy $ leftVal
                   then pure leftVal
                   else interpretExpr right
      _ -> RTError.throwRT op $ "AST Error: Operator "
                             <> tokenLexeme op
                             <> " not supported in logical position"

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         ) => ExprInterpreter cell (AST.Binary f) m where
  interpretExpr (AST.Binary left op right) = do
      leftVal <- interpretExpr left
      rightVal <- interpretExpr right
      case tokenType op of
        Token.COMMA         -> pure rightVal
        Token.PLUS          -> sumVals op leftVal rightVal
        Token.MINUS         -> applyNumericOp op (-) leftVal rightVal
        Token.STAR          -> applyNumericOp op (*) leftVal rightVal
        Token.SLASH         -> applyNumericOp op (/) leftVal rightVal
        Token.GREATER       -> applyComparisonOp op (>)  leftVal rightVal
        Token.GREATER_EQUAL -> applyComparisonOp op (>=) leftVal rightVal
        Token.LESS          -> applyComparisonOp op (<)  leftVal rightVal
        Token.LESS_EQUAL    -> applyComparisonOp op (<=) leftVal rightVal
        Token.EQUAL_EQUAL   -> pure . ValBool       $ isEqual leftVal rightVal
        Token.BANG_EQUAL    -> pure . ValBool . not $ isEqual leftVal rightVal
        _ -> RTError.throwRT op $ "AST Error: Operator "
                               <> tokenLexeme op
                               <> " not supported in binary position"
    where
      applyNumericOp    opTk op v1 v2 = ValNum  . uncurry op <$> numericOperands opTk v1 v2
      applyComparisonOp opTk op v1 v2 = ValBool . uncurry op <$> numericOperands opTk v1 v2
      sumVals _ (ValNum d1)    (ValNum d2)    = pure $ ValNum (d1 + d2)
      sumVals _ (ValString s1) (ValString s2) = pure $ ValString (s1 <> s2)
      sumVals opTk _ _ = RTError.throwRT opTk "Operands must be two numbers or two strings."

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         ) => ExprInterpreter cell (AST.Unary f) m where
  interpretExpr (AST.Unary op expr) = do
    val <- interpretExpr @cell expr
    case tokenType op of
      Token.BANG -> pure . ValBool . not . isTruthy $ val
      Token.MINUS -> ValNum . negate <$> numericOperand op val
      _ -> RTError.throwRT op $ "AST Error: Operator "
                             <> tokenLexeme op
                             <> " not supported in unary position"

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         ) => ExprInterpreter cell (AST.Grouping f) m where
  interpretExpr (AST.Grouping expr) = interpretExpr expr

instance Runtime cell sig m => ExprInterpreter cell (AST.Function RuntimeAST) m where
  interpretExpr fn = do
    frame <- gets (RTState.localFrame @cell)
    pure . ValFn $ LoxFn fn frame False

instance Runtime cell sig m => ExprInterpreter cell AST.Literal m where
  interpretExpr (AST.LitString s) = pure $ ValString s
  interpretExpr (AST.LitNum d)    = pure $ ValNum d
  interpretExpr (AST.LitBool b)   = pure $ ValBool b
  interpretExpr AST.LitNil        = pure $ ValNil

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         ) => ExprInterpreter cell (AST.Get f) m where
  interpretExpr (AST.Get objectExpr tk) = do
    object <- interpretExpr objectExpr
    case object of
      ValInstance inst -> getProperty @cell inst tk
      _ -> RTError.throwRT tk "Only instances have properties."

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         ) => ExprInterpreter cell (AST.SetProperty f) m where
  interpretExpr (AST.SetProperty objectExpr tk valueExpr) = do
    object <- interpretExpr objectExpr
    case object of
      ValInstance inst -> do
        value <- interpretExpr valueExpr
        setProperty @cell inst tk value
      _ -> RTError.throwRT tk "Only instances have properties."

getProperty :: forall cell sig m
             . Runtime cell sig m
            => LoxInstance cell
            -> Token
            -> m (RTValue cell)
getProperty inst@(LoxInstance klass properties) propertyName = do
  let name = tokenLexeme propertyName
  props <- Cells.readCell properties
  case Map.lookup name props of
    Just val -> pure val
    Nothing -> do
      case findMethod name klass of
        Just method -> ValFn <$> bindThis inst method
        Nothing -> RTError.throwRT propertyName $ "Undefined property '" <> name <> "'."

findMethod :: T.Text -> LoxClass cell -> Maybe (LoxFn cell)
findMethod name (LoxClass _ methodTable) = Map.lookup name methodTable

bindThis :: Runtime cell sig m
         => LoxInstance cell -> LoxFn cell -> m (LoxFn cell)
bindThis inst (LoxFn function env isInit) = RTState.runInChildEnvOf env $ do
  RTState.defineM "this" (ValInstance inst)
  LoxFn function <$> gets RTState.localFrame <*> pure isInit

setProperty :: forall cell sig m
             . Runtime cell sig m
            => LoxInstance cell
            -> Token
            -> RTValue cell
            -> m (RTValue cell)
setProperty (LoxInstance _klass properties) propertyName value = do
  let name = tokenLexeme propertyName
  Cells.updateCell (Map.insert name value) properties
  pure value

instance ( Runtime cell sig m
         , ExprInterpreter cell (AST.Expr f) m
         ) => ExprInterpreter cell (AST.Call f) m where
  interpretExpr (AST.Call calleeExpr paren argExprs) = do
    callee <- interpretExpr @cell calleeExpr
    args <- traverse interpretExpr argExprs
    case callee of
      ValFn fn -> call paren fn args
      ValClass klass -> call paren klass args
      ValNativeFn nativeFn -> call paren nativeFn args
      _ -> RTError.throwRT paren "Can only call functions and classes."

call :: forall cell e sig m
      . LoxCallable cell e m
     => Has (Throw RTError) sig m
     => Token -> e -> Seq (RTValue cell) -> m (RTValue cell)
call paren callee args = do
  arity <- loxArity @cell callee
  let argCount = Seq.length args
  when (arity /= argCount) $
    RTError.throwRT paren $ "Expected "
                         <> T.pack (show arity)
                         <> " arguments, but got "
                         <> T.pack (show argCount)
                         <> "."
  loxCall paren callee args

class LoxCallable cell e m where
  loxArity :: e -> m Int
  loxCall :: Token -> e -> Seq (RTValue cell) -> m (RTValue cell)

instance Runtime cell sig m => LoxCallable cell (LoxFn cell) m where
  loxArity (LoxFn (AST.Function _ params _) _ _) = pure (Seq.length params)
  loxCall _ (LoxFn (AST.Function _ params body) env isInit) args = do
    returnVal <- RTReturn.catchReturn $
                  RTState.runInChildEnvOf env $ do
                    for_ (Seq.zip params args) $ \(param, arg) -> do
                      RTState.defineM (tokenLexeme param) arg
                    interpretStmt @cell body
                    pure ValNil
    if isInit
    then RTState.runInChildEnvOf env $ do
      RTState.getBoundValueAtM (Token "this" Token.THIS Nothing 0) (Just 1)
    else pure returnVal

instance Runtime cell sig m => LoxCallable cell (LoxClass cell) m where
  loxArity klass = case findMethod "init" klass of
    Nothing -> pure 0
    Just fn -> loxArity @cell fn
  loxCall tk klass@(LoxClass _name _methods) args = do
    instanceState <- Cells.newCell @cell (Map.empty)
    let inst = LoxInstance klass instanceState
    case findMethod "init" klass of
      Nothing -> pure ()
      Just init -> do
        boundInit <- bindThis inst init
        call tk boundInit args
        pure ()
    pure $ ValInstance inst

instance Runtime cell sig m => LoxCallable cell LoxNativeFn m where
  loxArity (LoxNativeFn arity _) = pure arity
  loxCall tk (LoxNativeFn _ impl) args = runNativeFnImpl impl tk args

isTruthy :: RTValue cell -> Bool
isTruthy (ValBool b) = b
isTruthy ValNil = False
isTruthy _ = True

isEqual :: RTValue cell -> RTValue cell -> Bool
isEqual (ValString s1) (ValString s2) = s1 == s2
isEqual (ValNum d1)    (ValNum d2)    = d1 == d2
isEqual (ValBool b1)   (ValBool b2)   = b1 == b2
isEqual ValNil         ValNil         = True
isEqual _              _              = False

numericOperand :: Has (Throw RTError) sig m
                => Token
                -> RTValue cell
                -> m Double
numericOperand _ (ValNum n) = pure n
numericOperand opTk _ = RTError.throwRT opTk "Operand must be a number."

numericOperands :: Has (Throw RTError) sig m
                => Token
                -> RTValue cell
                -> RTValue cell
                -> m (Double, Double)
numericOperands _ (ValNum n1) (ValNum n2) = pure (n1, n2)
numericOperands opTk _ _ = RTError.throwRT opTk "Operands must be numbers."
