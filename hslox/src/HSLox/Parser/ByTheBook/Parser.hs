module HSLox.Parser.ByTheBook.Parser where

import qualified Control.Effect.Error as ErrorEff
import Control.Carrier.Lift
import Control.Carrier.State.Church
import Control.Carrier.Writer.Church
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import HSLox.AST
import HSLox.Parser.ParserError
import HSLox.Parser.ByTheBook.ParserState
import HSLox.Token (Token (..), TokenType)
import qualified HSLox.Token as Token
import qualified HSLox.Util as Util

parse :: forall sig m. Has (Writer (Seq ParserError)) sig m
                    => (Seq Token)
                    -> m (Seq Expr)
parse tokens
  = evalState (initialParserState tokens)
  . execWriter @(Seq Expr)
  $ do
    expr' <- Util.runErrorToEither @ParserError expression
    case expr' of
      Left error -> reportError error
      Right expr -> tell $ Seq.singleton expr

type ExprParser = forall sig m. ( Has (ErrorEff.Throw ParserError) sig m
                                , Has (State ParserState) sig m )
                                => m Expr

expression :: ExprParser
expression = equality

equality :: ExprParser
equality = leftAssociativeBinaryOp comparison [ Token.EQUAL_EQUAL
                                              , Token.BANG_EQUAL
                                              ]

comparison :: ExprParser
comparison = leftAssociativeBinaryOp addition [ Token.GREATER
                                              , Token.GREATER_EQUAL
                                              , Token.LESS
                                              , Token.LESS_EQUAL
                                              ]

addition :: ExprParser
addition = leftAssociativeBinaryOp multiplication [ Token.MINUS
                                                  , Token.PLUS
                                                  ]

multiplication :: ExprParser
multiplication = leftAssociativeBinaryOp unary [ Token.STAR
                                               , Token.SLASH
                                               ]

unary :: ExprParser
unary = do
  mbTk <- Util.runEmptyToMaybe (match [Token.MINUS, Token.BANG])
  case mbTk of
    Nothing -> primary
    Just tk -> UnaryE tk <$> unary

primary :: ExprParser
primary = do
    mbTk <- Util.runEmptyToMaybe (match [ Token.FALSE
                                        , Token.TRUE
                                        , Token.NIL
                                        , Token.NUMBER
                                        , Token.STRING
                                        , Token.LEFT_PAREN
                                        ])
    case mbTk of
      Just (Token { tokenType = Token.FALSE }) -> pure (BoolE False)
      Just (Token { tokenType = Token.TRUE })  -> pure (BoolE True)
      Just (Token { tokenType = Token.NIL })   -> pure NilE
      Just (Token { tokenType = Token.STRING, tokenLiteral }) ->
        case tokenLiteral of
          Just (Token.LitString s) -> pure (StringE s)
          _ -> error "SCANNER ERROR: Expected string literal in STRING token."
      Just (Token { tokenType = Token.NUMBER, tokenLiteral }) ->
        case tokenLiteral of
          Just (Token.LitNum n) -> pure (NumE n)
          _ -> error "SCANNER ERROR: Expected numeric literal in NUMBER token."
      Just (Token { tokenType = Token.LEFT_PAREN }) -> do
        expr <- expression
        match [Token.RIGHT_PAREN]
          `Util.recoverFromEmptyWith`
          error "Expect ')' after expression."
        pure expr
      _ -> error "Expect expression."
  where
    error msg = do
      line <- currentLine
      ErrorEff.throwError $ ParserError line "" msg

leftAssociativeBinaryOp :: Foldable t => ExprParser -> t TokenType -> ExprParser
leftAssociativeBinaryOp termParser opTypes = do
  left <- termParser
  e <- execState left . Util.untilEmpty $ do
    left <- get
    op <- match opTypes
    right <- termParser
    put $ BinaryE left op right
  pure e
