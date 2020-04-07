{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}
module HSLox.TreeWalk.Interpreter
  ( interpretExprs
  , RTValue (..)
  , RTError (..)
  , showValue
  ) where

import Control.Carrier.Error.Church
import Control.Effect.Writer
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified HSLox.AST as AST
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token
import qualified HSLox.Util as Util

data RTValue
  = ValString T.Text
  | ValNum Double
  | ValBool Bool
  | ValNil
  deriving (Eq, Show, Ord)

data RTError = RTError { rtErrorMessage :: T.Text
                       , rtErrorToken :: Token
                       }
  deriving (Eq, Show)

interpretExprs :: Foldable t
               => Algebra sig m
               => t AST.Expr
               -> m (Seq RTValue, Maybe RTError)
interpretExprs = collectingValuesAndError . evaluateExprs
  where
    collectingValuesAndError = Util.runWriterToPair @(Seq RTValue)
                             . fmap (Util.rightToMaybe . Util.swapEither)
                             . Util.runErrorToEither @RTError
    evaluateExprs exprs = for_ exprs $ \expr -> do
      result <- interpretAST expr
      tell (Seq.singleton result)

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

class ASTInterpreter e m where
  interpretAST :: e -> m RTValue

instance Has (Error RTError) sig m
      => ASTInterpreter AST.Expr m where
  interpretAST (AST.UnaryExpr t) = interpretAST t
  interpretAST (AST.BinaryExpr t) = interpretAST t
  interpretAST (AST.TernaryExpr t) = interpretAST t
  interpretAST (AST.GroupingExpr t) = interpretAST t
  interpretAST (AST.LiteralExpr t) = interpretAST t

instance ( ASTInterpreter AST.Expr m
         , Has (Error RTError) sig m )
      => ASTInterpreter AST.Ternary m where
  interpretAST (AST.Ternary left op1 middle op2 right) = do
      leftVal <- interpretAST left
      case (tokenType op1, tokenType op2) of
        (Token.QUESTION_MARK, Token.COLON) ->
          if isTruthy leftVal
          then interpretAST middle
          else interpretAST right
        _ -> throwRT op2 $ "AST Error: Operator pair "
                        <> tokenLexeme op1
                        <> " and "
                        <> tokenLexeme op2
                        <> " not supported in ternary position"

instance ( ASTInterpreter AST.Expr m
         , Has (Error RTError) sig m )
      => ASTInterpreter AST.Binary m where
  interpretAST (AST.Binary left op right) = do
      leftVal <- interpretAST left
      rightVal <- interpretAST right
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
        _ -> throwRT op $ "AST Error: Operator "
                       <> tokenLexeme op
                       <> " not supported in binary position"
    where
      applyNumericOp    opTk op v1 v2 = ValNum  . uncurry op <$> numericOperands opTk v1 v2
      applyComparisonOp opTk op v1 v2 = ValBool . uncurry op <$> numericOperands opTk v1 v2
      sumVals _ (ValNum d1)    (ValNum d2)    = pure $ ValNum (d1 + d2)
      sumVals _ (ValString s1) (ValString s2) = pure $ ValString (s1 <> s2)
      sumVals opTk _ _ = throwRT opTk "Operands must be two numbers or two strings."

instance ( ASTInterpreter AST.Expr m
         , Has (Error RTError) sig m )
      => ASTInterpreter AST.Unary m where
  interpretAST (AST.Unary op expr) = do
    val <- interpretAST expr
    case tokenType op of
      Token.BANG -> pure . ValBool . not . isTruthy $ val
      Token.MINUS -> ValNum . negate <$> numericOperand op val
      _ -> throwRT op $ "AST Error: Operator "
                     <> tokenLexeme op
                     <> " not supported in unary position"

instance ASTInterpreter AST.Expr m => ASTInterpreter AST.Grouping m where
  interpretAST (AST.Grouping expr) = interpretAST expr

instance Applicative m => ASTInterpreter AST.Literal m where
  interpretAST (AST.LitString s) = pure $ ValString s
  interpretAST (AST.LitNum d)    = pure $ ValNum d
  interpretAST (AST.LitBool b)   = pure $ ValBool b
  interpretAST AST.LitNil        = pure $ ValNil

throwRT :: Has (Throw RTError) sig m => Token -> T.Text -> m a
throwRT tk msg = throwError $ RTError msg tk

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
numericOperand opTk _ = throwRT opTk "Operand must be a number."

numericOperands :: Has (Throw RTError) sig m
                => Token
                -> RTValue
                -> RTValue
                -> m (Double, Double)
numericOperands _ (ValNum n1) (ValNum n2) = pure (n1, n2)
numericOperands opTk _ _ = throwRT opTk "Operands must be numbers."
