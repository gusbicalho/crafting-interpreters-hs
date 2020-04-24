{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}
module HSLox.TreeWalk.Interpreter
  ( interpret
  , interpretNext
  , RTValue (..)
  , RTError (..)
  , RTState (..)
  , baseEnv
  ) where

import Control.Carrier.Error.Church
import Control.Carrier.State.Church
import Control.Monad (when)
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Kind (Type)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified HSLox.AST as AST
import qualified HSLox.Cells.Effect as Cells
import qualified HSLox.NativeFns.Effect as NativeFns
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
baseEnv = do
  empty <- RTState.newState
  execState empty $ do
    RTState.defineM @cell "clock" $ NativeDef 0 (\_ _ ->
      ValNum . fromIntegral <$> NativeFns.clock)
    RTState.defineM @cell "print" $ NativeDef 1 (\_ args -> case args of
      arg :<| _ -> do
        NativeFns.printText (showValue arg)
        pure ValNil
      _ -> pure ValNil)

interpret :: forall cell sig m
           . Has (Cells.Cells cell) sig m
          => Has NativeFns.NativeFns sig m
          => AST.Program -> m (Maybe RTError)
interpret prog = do
  env <- baseEnv @cell
  (_, rtError) <- interpretNext env prog
  pure rtError
{-# INLINE interpret #-}

interpretNext :: forall cell sig m
               . Has (Cells.Cells cell) sig m
              => Has NativeFns.NativeFns sig m
              => RTState cell
              -> AST.Program
              -> m (RTState cell, Maybe RTError)
interpretNext env prog = prog & interpretStmt @cell
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
showValue (ValFn (LoxFn (AST.Function tk _ _) _)) = "<fn " <> tokenLexeme tk <> ">"
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

instance Runtime cell sig m => StmtInterpreter cell AST.Program m where
  interpretStmt (AST.Program stmts) = for_ stmts (interpretStmt @cell)

instance Runtime cell sig m => StmtInterpreter cell AST.Stmt m where
  interpretStmt (AST.ExprStmt expr) = interpretExpr @cell expr $> ()
  interpretStmt (AST.VarDeclarationStmt decl) = interpretStmt @cell decl
  interpretStmt (AST.BlockStmt block) = interpretStmt @cell block
  interpretStmt (AST.IfStmt ifStmt) = interpretStmt @cell ifStmt
  interpretStmt (AST.WhileStmt whileStmt) = interpretStmt @cell whileStmt
  interpretStmt (AST.FunctionVarDeclarationStmt function) = interpretStmt @cell function
  interpretStmt (AST.ReturnStmt return) = interpretStmt @cell return

instance Runtime cell sig m => StmtInterpreter cell AST.VarDeclaration m where
  interpretStmt (AST.VarDeclaration tk expr) = do
    val <- interpretExpr @cell expr
    RTState.defineM (tokenLexeme tk) val

instance Runtime cell sig m => StmtInterpreter cell AST.Block m where
  interpretStmt (AST.Block stmts) =
    RTState.runInChildEnv @cell $ do
      for_ stmts (interpretStmt @cell)

instance Runtime cell sig m => StmtInterpreter cell AST.If m where
  interpretStmt (AST.If cond thenStmt elseStmt) = do
    cond <- interpretExpr @cell cond
    if isTruthy cond
    then interpretStmt @cell thenStmt
    else maybe (pure ()) (interpretStmt @cell) elseStmt

instance Runtime cell sig m => StmtInterpreter cell AST.While m where
  interpretStmt (AST.While cond body) =
    Util.whileM (isTruthy <$> interpretExpr @cell cond) $
      interpretStmt @cell body

instance Runtime cell sig m => StmtInterpreter cell AST.Function m where
  interpretStmt fn@(AST.Function tk _ _) = do
    frame <- gets (RTState.rtStateLocalFrame @cell)
    RTState.defineM (tokenLexeme tk) (ValFn $ LoxFn fn frame)

instance Runtime cell sig m => StmtInterpreter cell AST.Return m where
  interpretStmt (AST.Return tk expr) = do
    val <- interpretExpr @cell expr
    RTReturn.throwReturn tk val

class ExprInterpreter (cell :: Type -> Type)
                      (e :: Type)
                      (m :: Type -> Type) where
  interpretExpr :: e -> m (RTValue cell)

instance Runtime cell sig m => ExprInterpreter cell AST.Expr m where
  interpretExpr (AST.UnaryExpr t) = interpretExpr t
  interpretExpr (AST.LogicalExpr t) = interpretExpr t
  interpretExpr (AST.BinaryExpr t) = interpretExpr t
  interpretExpr (AST.TernaryExpr t) = interpretExpr t
  interpretExpr (AST.GroupingExpr t) = interpretExpr t
  interpretExpr (AST.LiteralExpr t) = interpretExpr t
  interpretExpr (AST.VariableExpr t) = interpretExpr t
  interpretExpr (AST.AssignmentExpr t) = interpretExpr t
  interpretExpr (AST.CallExpr t) = interpretExpr t

instance Runtime cell sig m => ExprInterpreter cell AST.Ternary m where
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

instance Runtime cell sig m => ExprInterpreter cell AST.Logical m where
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

instance Runtime cell sig m => ExprInterpreter cell AST.Binary m where
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

instance Runtime cell sig m => ExprInterpreter cell AST.Unary m where
  interpretExpr (AST.Unary op expr) = do
    val <- interpretExpr @cell expr
    case tokenType op of
      Token.BANG -> pure . ValBool . not . isTruthy $ val
      Token.MINUS -> ValNum . negate <$> numericOperand op val
      _ -> RTError.throwRT op $ "AST Error: Operator "
                             <> tokenLexeme op
                             <> " not supported in unary position"

instance Runtime cell sig m => ExprInterpreter cell AST.Grouping m where
  interpretExpr (AST.Grouping expr) = interpretExpr expr

instance Runtime cell sig m => ExprInterpreter cell AST.Literal m where
  interpretExpr (AST.LitString s) = pure $ ValString s
  interpretExpr (AST.LitNum d)    = pure $ ValNum d
  interpretExpr (AST.LitBool b)   = pure $ ValBool b
  interpretExpr AST.LitNil        = pure $ ValNil

instance Runtime cell sig m => ExprInterpreter cell AST.Variable m where
  interpretExpr (AST.Variable tk) = RTState.getBoundValueM tk

instance Runtime cell sig m => ExprInterpreter cell AST.Assignment m where
  interpretExpr (AST.Assignment tk expr) = do
    val <- interpretExpr expr
    RTState.assignM tk val
    pure val

instance Runtime cell sig m => ExprInterpreter cell AST.Call m where
  interpretExpr (AST.Call calleeExpr paren argExprs) = do
      callee <- interpretExpr @cell calleeExpr
      args <- traverse interpretExpr argExprs
      case callee of
        ValFn fn -> call paren fn args
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
  loxArity (LoxFn (AST.Function _ params _) _) = pure (Seq.length params)
  loxCall _ (LoxFn (AST.Function _ params body) env) args = do
    RTReturn.catchReturn $
      RTState.runInChildEnvOf env $ do
        for_ (Seq.zip params args) $ \(param, arg) -> do
          RTState.defineM (tokenLexeme param) arg
        interpretStmt @cell body
        pure ValNil

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
