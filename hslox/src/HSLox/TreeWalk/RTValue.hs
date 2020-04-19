module HSLox.TreeWalk.RTValue where

import Control.Effect.Error
import Data.Sequence (Seq (..))
import qualified Data.Text as T
import HSLox.Output.Effect
import HSLox.Token (Token)
import HSLox.TreeWalk.RTError (RTError)

data RTValue
  = ValString T.Text
  | ValNum Double
  | ValBool Bool
  | ValNil
  | ValFn LoxFn
  | ValNativeFn LoxNativeFn
  deriving (Show)

data LoxFn = LoxFn { loxFnArity :: Int }
  deriving (Eq, Show, Ord)

data LoxNativeFn = LoxNativeFn { loxNativeFnArity :: Int
                               , loxNativeFnName :: T.Text
                               , loxNativeFnNameImpl :: NativeFnImpl
                               }
  deriving (Show)

newtype NativeFnImpl
  = NativeFnImpl { runNativeFnImpl :: forall sig m
                                    . ( Has (Throw RTError) sig m
                                      , Has (Output RTValue) sig m )
                                   => Token -> Seq RTValue -> m RTValue }

instance Show NativeFnImpl where
  show _ = "<NativeFnImpl>"
