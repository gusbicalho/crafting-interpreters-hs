module HSLox.TreeWalk.Token where

import qualified Data.Text as T
import HSLox.TreeWalk.TokenType (TokenType)

data LiteralValue
  = LitString T.Text
  | LitBool Bool
  | LitNum Double
  deriving (Eq, Show)

data Token
  = Token
  { tokenLexeme :: T.Text
  , tokenType :: TokenType
  , tokenLiteral :: Maybe LiteralValue
  , tokenLine :: Int
  }
  deriving (Eq, Show)
