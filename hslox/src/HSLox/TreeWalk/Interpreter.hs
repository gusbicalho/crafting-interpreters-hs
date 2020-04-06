{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}
module HSLox.TreeWalk.Interpreter where

import Control.Carrier.Error.Church
import Control.Effect.Trace
import Control.Effect.Writer
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified HSLox.AST as AST
import HSLox.ASTPrinter (printAST)
import HSLox.ErrorReport (ErrorReport, toErrorReport)
import qualified HSLox.Scanner.Megaparsec as Scanner
import HSLox.Scanner.ScanError (ScanError)
import qualified HSLox.Parser.Megaparsec as Parser
import HSLox.Parser.ParserError (ParserError)
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token
import qualified HSLox.Util as Util

interpret :: Has Trace sig m
          => T.Text -> m (Either (Seq ErrorReport) (Seq RTValue, Maybe RTError))
interpret source = do
  let parsed = runParser source
  case parsed of
    Left (scanErrors, parserErrors) ->
      pure . Left $ (toErrorReport <$> scanErrors)
                 <> (toErrorReport <$> parserErrors)
    Right exprs ->
      fmap Right $ Util.runWriterToPair @(Seq RTValue)
                 . fmap (Util.rightToMaybe . Util.swapEither)
                 . Util.runErrorToEither @RTError
                 . for_ exprs $ \expr -> do
                    trace . T.unpack $ "expr> " <> printAST expr
                    result <- interpretAST expr
                    tell (Seq.singleton result)

runParser :: T.Text -> Either (Seq ScanError, Seq ParserError) (Seq AST.Expr)
runParser source = run $ do
  (scanErrors, tokens) <- Util.runWriterToPair @(Seq ScanError) $ Scanner.scanTokens source
  (parserErrors, exprs) <- Util.runWriterToPair @(Seq ParserError) $ Parser.parse tokens
  if (scanErrors /= Seq.empty || parserErrors /= Seq.empty)
  then pure . Left  $ (scanErrors, parserErrors)
  else pure . Right $ exprs

data RTValue
  = ValString T.Text
  | ValNum Double
  | ValBool Bool
  | ValNil
  deriving (Eq, Show, Ord)

data RTError = RTError T.Text
  deriving (Eq, Show)

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
        _ -> throwRT $ "AST Error: Operator pair "
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
        Token.PLUS          -> sumVals leftVal rightVal
        Token.MINUS         -> applyNumericOp (-) leftVal rightVal
        Token.STAR          -> applyNumericOp (*) leftVal rightVal
        Token.SLASH         -> applyNumericOp (/) leftVal rightVal
        Token.GREATER       -> applyComparisonOp (>)  leftVal rightVal
        Token.GREATER_EQUAL -> applyComparisonOp (>=) leftVal rightVal
        Token.LESS          -> applyComparisonOp (<)  leftVal rightVal
        Token.LESS_EQUAL    -> applyComparisonOp (<=) leftVal rightVal
        Token.EQUAL_EQUAL   -> pure . ValBool       $ isEqual leftVal rightVal
        Token.BANG_EQUAL    -> pure . ValBool . not $ isEqual leftVal rightVal
        _ -> throwRT $ "AST Error: Operator "
                    <> tokenLexeme op
                    <> " not supported in binary position"
    where
      applyNumericOp op v1 v2 = ValNum <$> (op <$> castValToNum v1
                                               <*> castValToNum v2)
      sumVals (ValNum d1)    v2 = ValNum    . (d1 +)  <$> castValToNum v2
      sumVals (ValString s1) v2 = ValString . (s1 <>) <$> castValToString v2
      sumVals _              _  = throwRT "Operands must be two numbers or two strings."
      applyComparisonOp op v1 v2 = ValBool <$> (op <$> castValToNum v1
                                                   <*> castValToNum v2)
      -- sumVals (ValNum d1) (ValNum d2) = pure . ValNum $ d1 - d2

instance ( ASTInterpreter AST.Expr m
         , Has (Error RTError) sig m )
      => ASTInterpreter AST.Unary m where
  interpretAST (AST.Unary op expr) = do
    val <- interpretAST expr
    case tokenType op of
      Token.BANG -> pure . ValBool . not . isTruthy $val
      Token.MINUS -> negateValue val
      _ -> throwRT $ "AST Error: Operator "
                  <> tokenLexeme op
                  <> " not supported in unary position"

instance ASTInterpreter AST.Expr m => ASTInterpreter AST.Grouping m where
  interpretAST (AST.Grouping expr) = interpretAST expr

instance Applicative m => ASTInterpreter AST.Literal m where
  interpretAST (AST.LitString s) = pure $ ValString s
  interpretAST (AST.LitNum d)    = pure $ ValNum d
  interpretAST (AST.LitBool b)   = pure $ ValBool b
  interpretAST AST.LitNil        = pure $ ValNil

throwRT :: Has (Error RTError) sig m => T.Text -> m a
throwRT = throwError . RTError

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

negateValue :: Has (Error RTError) sig m => RTValue -> m RTValue
negateValue (ValNum d) = pure $ ValNum (negate d)
negateValue v = throwRT $ "Unary - expected number operand, found " <> T.pack (show v)

castValToNum :: Has (Error RTError) sig m => RTValue -> m Double
castValToNum (ValNum d) = pure d
castValToNum v = throwRT $ "Expected number, found " <> T.pack (show v)

castValToString :: Has (Error RTError) sig m => RTValue -> m T.Text
castValToString (ValString s) = pure s
castValToString v = throwRT $ "Expected string, found " <> T.pack (show v)
