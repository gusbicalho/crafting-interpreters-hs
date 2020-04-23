module HSLox.TreeWalk.Runtime
  ( Trace, trace
  , module HSLox.TreeWalk.Runtime
  ) where

import Control.Effect.Trace
import Control.Effect.Error
import Control.Effect.State
import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import qualified Data.Text as T
import qualified HSLox.AST as AST
import HSLox.NativeFns.Effect
import HSLox.Token (Token (..))

type Runtime sig m = ( Has NativeFns sig m
                     , Has (Error RTError) sig m
                     , Has (Error RTReturn) sig m
                     , Has (State RTState) sig m
                     )

type BindingName = T.Text

newtype RTEnv = RTEnv { rtEnvBindings :: Map BindingName RTValue }
  deriving (Show)

data RTFrame = RTFrame { rtFrameEnv :: RTEnv
                       , rtFrameEnclosing :: Maybe RTFrame
                       }

instance Show RTFrame where
  show _ = "<stack frame>"

data RTState = RTState { rtStateGlobalEnv :: RTEnv
                       , rtStateLocalFrame :: Maybe RTFrame
                       }

data RTReturn = RTReturn Token RTValue

data RTError = RTError { rtErrorMessage :: T.Text
                       , rtErrorToken :: Token
                       }
  deriving (Eq, Show)

data RTValue
  = ValString T.Text
  | ValNum Double
  | ValBool Bool
  | ValNil
  | ValFn LoxFn
  | ValNativeFn LoxNativeFn
  deriving (Show)

newtype LoxFn = LoxFn { loxFnAST :: AST.Function }
  deriving (Show)

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
