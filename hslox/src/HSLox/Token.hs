module HSLox.Token where

import qualified Data.Text as T

data LiteralValue
  = LitString T.Text
  | LitNum Double
  deriving (Eq, Show, Ord)

data Token
  = Token
  { tokenLexeme :: T.Text
  , tokenType :: TokenType
  , tokenLiteral :: Maybe LiteralValue
  , tokenLine :: Int
  }
  deriving (Eq, Show, Ord)

data TokenType
  -- Single-character tokens
  = LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COLON
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | QUESTION_MARK
  | SEMICOLON
  | SLASH
  | STAR
  -- One or two character tokens
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  -- Literals
  | IDENTIFIER
  | STRING
  | NUMBER
  -- Keywords
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  -- EOF
  | EOF
  deriving (Eq, Ord, Enum, Bounded, Show)
