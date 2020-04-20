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
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified HSLox.AST as AST
import qualified HSLox.NativeFns.Effect as NativeFns
import HSLox.Output.Carrier.Transform
import HSLox.Output.Effect
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token
import HSLox.TreeWalk.RTState (RTState (..))
import qualified HSLox.TreeWalk.RTState as RTState
import HSLox.TreeWalk.RTError (RTError (..))
import qualified HSLox.TreeWalk.RTError as RTError
import HSLox.TreeWalk.RTValue (RTValue (..), LoxFn (..), LoxNativeFn (..), pattern NativeDef, runNativeFnImpl)
import HSLox.TreeWalk.Runtime
import qualified HSLox.Util as Util

baseEnv :: Algebra sig m => m RTState
baseEnv = execState RTState.newState $ do
  RTState.defineM "clock" $ NativeDef 0 (\_ _ ->
    ValNum . fromIntegral <$> NativeFns.clock)

interpret :: Has NativeFns.NativeFns sig m
          => Has (Output T.Text) sig m
          => AST.Program -> m (Maybe RTError)
interpret prog = do
  env <- baseEnv
  (_, rtError) <- interpretNext env prog
  pure rtError

interpretNext :: Has NativeFns.NativeFns sig m
              => Has (Output T.Text) sig m
              => RTState
              -> AST.Program
              -> m (RTState, Maybe RTError)
interpretNext env prog = prog & interpretStmt
                              & Util.runErrorToEither @RTError
                              & fmap (Util.rightToMaybe . Util.swapEither)
                              & runOutputTransform showValue
                              & Util.runStateToPair env

showValue :: RTValue -> T.Text
showValue (ValString s) = s
showValue (ValBool True) = "true"
showValue (ValBool False) = "false"
showValue ValNil = "nil"
showValue (ValFn fn) = T.pack $ show fn
showValue (ValNativeFn fn) = T.pack $ show fn
showValue (ValNum d) = dropZeroDecimal doubleString
  where
    doubleString = T.pack $ show d
    dropZeroDecimal numStr
      | T.takeEnd 2 numStr == ".0" = T.dropEnd 2 numStr
      | otherwise                  = numStr

class StmtInterpreter e m where
  interpretStmt :: e -> m ()

instance Runtime sig m => StmtInterpreter AST.Program m where
  interpretStmt (AST.Program stmts) = for_ stmts interpretStmt

instance Runtime sig m => StmtInterpreter AST.Stmt m where
  interpretStmt (AST.ExprStmt expr) = interpretExpr expr $> ()
  interpretStmt (AST.PrintStmt stmt) = interpretStmt stmt
  interpretStmt (AST.DeclarationStmt decl) = interpretStmt decl
  interpretStmt (AST.BlockStmt block) = interpretStmt block
  interpretStmt (AST.IfStmt ifStmt) = interpretStmt ifStmt
  interpretStmt (AST.WhileStmt whileStmt) = interpretStmt whileStmt

instance Runtime sig m => StmtInterpreter AST.Print m where
  interpretStmt (AST.Print _ expr) = output =<< interpretExpr expr

instance Runtime sig m => StmtInterpreter AST.Declaration m where
  interpretStmt (AST.VarDeclaration tk expr) = do
    val <- interpretExpr expr
    RTState.defineM (tokenLexeme tk) val

instance Runtime sig m => StmtInterpreter AST.Block m where
  interpretStmt (AST.Block stmts) =
    RTState.runInChildEnv $ for_ stmts interpretStmt

instance Runtime sig m => StmtInterpreter AST.If m where
  interpretStmt (AST.If cond thenStmt elseStmt) = do
    cond <- interpretExpr cond
    if isTruthy cond
    then interpretStmt thenStmt
    else maybe (pure ()) interpretStmt elseStmt

instance Runtime sig m => StmtInterpreter AST.While m where
  interpretStmt (AST.While cond body) =
    Util.whileM (isTruthy <$> interpretExpr cond) $
      interpretStmt body

class ExprInterpreter e m where
  interpretExpr :: e -> m RTValue

instance Runtime sig m => ExprInterpreter AST.Expr m where
  interpretExpr (AST.UnaryExpr t) = interpretExpr t
  interpretExpr (AST.LogicalExpr t) = interpretExpr t
  interpretExpr (AST.BinaryExpr t) = interpretExpr t
  interpretExpr (AST.TernaryExpr t) = interpretExpr t
  interpretExpr (AST.GroupingExpr t) = interpretExpr t
  interpretExpr (AST.LiteralExpr t) = interpretExpr t
  interpretExpr (AST.VariableExpr t) = interpretExpr t
  interpretExpr (AST.AssignmentExpr t) = interpretExpr t
  interpretExpr (AST.CallExpr t) = interpretExpr t

instance Runtime sig m => ExprInterpreter AST.Ternary m where
  interpretExpr (AST.Ternary left op1 middle op2 right) = do
      leftVal <- interpretExpr left
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

instance Runtime sig m => ExprInterpreter AST.Logical m where
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

instance Runtime sig m => ExprInterpreter AST.Binary m where
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

instance Runtime sig m => ExprInterpreter AST.Unary m where
  interpretExpr (AST.Unary op expr) = do
    val <- interpretExpr expr
    case tokenType op of
      Token.BANG -> pure . ValBool . not . isTruthy $ val
      Token.MINUS -> ValNum . negate <$> numericOperand op val
      _ -> RTError.throwRT op $ "AST Error: Operator "
                             <> tokenLexeme op
                             <> " not supported in unary position"

instance Runtime sig m => ExprInterpreter AST.Grouping m where
  interpretExpr (AST.Grouping expr) = interpretExpr expr

instance Runtime sig m => ExprInterpreter AST.Literal m where
  interpretExpr (AST.LitString s) = pure $ ValString s
  interpretExpr (AST.LitNum d)    = pure $ ValNum d
  interpretExpr (AST.LitBool b)   = pure $ ValBool b
  interpretExpr AST.LitNil        = pure $ ValNil

instance Runtime sig m => ExprInterpreter AST.Variable m where
  interpretExpr (AST.Variable tk) = RTState.getBoundValueM tk

instance Runtime sig m => ExprInterpreter AST.Assignment m where
  interpretExpr (AST.Assignment tk expr) = do
    val <- interpretExpr expr
    RTState.assignM tk val
    pure val

instance Runtime sig m => ExprInterpreter AST.Call m where
  interpretExpr (AST.Call calleeExpr paren argExprs) = do
      callee <- interpretExpr calleeExpr
      args <- traverse interpretExpr argExprs
      case callee of
        ValFn fn -> call paren fn args
        ValNativeFn nativeFn -> call paren nativeFn args
        _ -> RTError.throwRT paren "Can only call functions and classes."

call :: LoxCallable e m
     => Has (Throw RTError) sig m
     => Token -> e -> Seq RTValue -> m RTValue
call paren callee args = do
  arity <- loxArity callee
  let argCount = Seq.length args
  when (arity /= argCount) $
    RTError.throwRT paren $ "Expected "
                         <> T.pack (show arity)
                         <> " arguments, but got "
                         <> T.pack (show argCount)
                         <> "."
  loxCall paren callee args

class LoxCallable e m where
  loxArity :: e -> m Int
  loxCall :: Token -> e -> Seq RTValue -> m RTValue

instance Runtime sig m => LoxCallable LoxFn m where
  loxArity (LoxFn arity) = pure arity
  loxCall tk (LoxFn _arity) _args = RTError.throwRT tk "Function calls are not implemented yet"

instance Runtime sig m => LoxCallable LoxNativeFn m where
  loxArity (LoxNativeFn arity _) = pure arity
  loxCall tk (LoxNativeFn _ impl) args = runNativeFnImpl impl tk args

isTruthy :: RTValue -> Bool
isTruthy (ValBool b) = b
isTruthy ValNil = False
isTruthy _ = True

isEqual :: RTValue -> RTValue -> Bool
isEqual (ValString s1) (ValString s2) = s1 == s2
isEqual (ValNum d1)    (ValNum d2)    = d1 == d2
isEqual (ValBool b1)   (ValBool b2)   = b1 == b2
isEqual ValNil         ValNil         = True
isEqual _              _              = False

numericOperand :: Has (Throw RTError) sig m
                => Token
                -> RTValue
                -> m Double
numericOperand _ (ValNum n) = pure n
numericOperand opTk _ = RTError.throwRT opTk "Operand must be a number."

numericOperands :: Has (Throw RTError) sig m
                => Token
                -> RTValue
                -> RTValue
                -> m (Double, Double)
numericOperands _ (ValNum n1) (ValNum n2) = pure (n1, n2)
numericOperands opTk _ _ = RTError.throwRT opTk "Operands must be numbers."
