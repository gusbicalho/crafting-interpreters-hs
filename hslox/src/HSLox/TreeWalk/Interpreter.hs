{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}
module HSLox.TreeWalk.Interpreter
  ( interpret
  , RTValue (..)
  , RTError (..)
  , RTEnv (..)
  , RTEnv.newEnv
  ) where

import Control.Carrier.Error.Church
import Control.Effect.State
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Data.Text as T
import qualified HSLox.AST as AST
import HSLox.Output.Carrier.Transform
import HSLox.Output.Effect
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token
import HSLox.TreeWalk.RTEnv (RTEnv (..))
import qualified HSLox.TreeWalk.RTEnv as RTEnv
import HSLox.TreeWalk.RTError (RTError (..))
import qualified HSLox.TreeWalk.RTError as RTError
import HSLox.TreeWalk.RTValue (RTValue (..))
import qualified HSLox.Util as Util

interpret :: Has (Output T.Text) sig m
          => RTEnv
          -> AST.Program
          -> m (RTEnv, Maybe RTError)
interpret env prog = prog & interpretStmt
                          & Util.runErrorToEither @RTError
                          & fmap (Util.rightToMaybe . Util.swapEither)
                          & runOutputTransform showValue
                          & Util.runStateToPair env

showValue :: RTValue -> T.Text
showValue (ValString s) = s
showValue (ValBool True) = "true"
showValue (ValBool False) = "false"
showValue ValNil = "nil"
showValue (ValNum d) = dropZeroDecimal doubleString
  where
    doubleString = T.pack $ show d
    dropZeroDecimal numStr
      | T.takeEnd 2 numStr == ".0" = T.dropEnd 2 numStr
      | otherwise                  = numStr

type Runtime sig m = ( Has (Error RTError) sig m
                     , Has (Output RTValue) sig m
                     , Has (State RTEnv) sig m
                     )

class StmtInterpreter e m where
  interpretStmt :: e -> m ()

instance Runtime sig m => StmtInterpreter AST.Program m where
  interpretStmt (AST.Program stmts) = for_ stmts interpretStmt

instance Runtime sig m => StmtInterpreter AST.Stmt m where
  interpretStmt (AST.ExprStmt expr) = interpretExpr expr $> ()
  interpretStmt (AST.PrintStmt stmt) = interpretStmt stmt
  interpretStmt (AST.DeclarationStmt decl) = interpretStmt decl

instance Runtime sig m => StmtInterpreter AST.Print m where
  interpretStmt (AST.Print _ expr) = output =<< interpretExpr expr

instance Runtime sig m => StmtInterpreter AST.Declaration m where
  interpretStmt (AST.VarDeclaration tk expr) = do
    val <- interpretExpr expr
    RTEnv.defineM (tokenLexeme tk) val

class ExprInterpreter e m where
  interpretExpr :: e -> m RTValue

instance Runtime sig m => ExprInterpreter AST.Expr m where
  interpretExpr (AST.UnaryExpr t) = interpretExpr t
  interpretExpr (AST.BinaryExpr t) = interpretExpr t
  interpretExpr (AST.TernaryExpr t) = interpretExpr t
  interpretExpr (AST.GroupingExpr t) = interpretExpr t
  interpretExpr (AST.LiteralExpr t) = interpretExpr t
  interpretExpr (AST.VariableExpr t) = interpretExpr t
  interpretExpr (AST.AssignmentExpr t) = interpretExpr t

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
  interpretExpr (AST.Variable tk) = RTEnv.getBoundValueM tk

instance Runtime sig m => ExprInterpreter AST.Assignment m where
  interpretExpr (AST.Assignment tk expr) = do
    val <- interpretExpr expr
    RTEnv.assignM tk val
    pure val

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
