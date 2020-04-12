module HSLox.TreeWalk.RTValue where

import qualified Data.Text as T

data RTValue
  = ValString T.Text
  | ValNum Double
  | ValBool Bool
  | ValNil
  | ValFn LoxFn
  deriving (Eq, Show, Ord)

data LoxFn = LoxFn
  deriving (Eq, Show, Ord)
