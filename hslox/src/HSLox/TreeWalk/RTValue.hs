module HSLox.TreeWalk.RTValue where

import Control.Effect.Error
import Data.Sequence (Seq (..))
import qualified Data.Text as T
import HSLox.NativeFns.Effect
import qualified HSLox.AST as AST
import HSLox.Token (Token)

data RTValue
  = ValString T.Text
  | ValNum Double
  | ValBool Bool
  | ValNil
  | ValFn LoxFn
  | ValNativeFn LoxNativeFn
  deriving (Show)

data LoxFn = LoxFn { loxFnAST :: AST.Function }
  deriving (Eq, Show, Ord)

pattern NativeDef :: Int
                  -> (forall sig m. NativeFnImplFn sig m)
                  -> RTValue
pattern NativeDef arity impl = ValNativeFn (LoxNativeFn arity (NativeFnImpl impl))

data LoxNativeFn = LoxNativeFn { loxNativeFnArity :: Int
                               , loxNativeFnNameImpl :: NativeFnImpl
                               }
  deriving (Show)

type NativeFnImplFn sig m = Has NativeFns sig m => Token -> Seq RTValue -> m RTValue

newtype NativeFnImpl
  = NativeFnImpl { runNativeFnImpl :: forall sig m. NativeFnImplFn sig m }

instance Show NativeFnImpl where
  show _ = "<NativeFnImpl>"
