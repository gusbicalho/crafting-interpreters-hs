{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
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
import HSLox.Cells.Effect
import HSLox.NativeFns.Effect
import HSLox.Token (Token (..))

type Runtime cell sig m = ( Has NativeFns sig m
                          , Has (Cells cell) sig m
                          , Has (Error RTError) sig m
                          , Has (Error (RTReturn cell)) sig m
                          , Has (State (RTState cell)) sig m
                          )

type BindingName = T.Text

newtype RTCell cell = RTCell { unRTCell :: cell (RTValue cell) }

newtype RTEnv cell = RTEnv { rtEnvBindings :: cell (Map BindingName (RTCell cell)) }

data RTFrame cell = RTFrame { rtFrameEnv :: RTEnv cell
                            , rtFrameEnclosing :: Maybe (RTFrame cell)
                            }

instance Show (RTFrame cell) where
  show _ = "<stack frame>"

data RTState cell = RTState { rtStateGlobalEnv :: (RTEnv cell)
                            , rtStateLocalFrame :: Maybe (RTFrame cell)
                            }

data RTReturn cell = RTReturn Token (RTValue cell)

data RTError = RTError { rtErrorMessage :: T.Text
                       , rtErrorToken :: Token
                       }
  deriving (Eq, Show)

data RTValue cell
  = ValString T.Text
  | ValNum Double
  | ValBool Bool
  | ValNil
  | ValFn (LoxFn cell)
  | ValNativeFn LoxNativeFn
  deriving (Show)

data LoxFn  cell = LoxFn { loxFnAST :: AST.Function
                         , loxClosedEnv :: Maybe (RTFrame cell)
                         }
  deriving (Show)

pattern NativeDef :: Int
                  -> (forall cell sig m. NativeFnImplFn cell sig m)
                  -> (RTValue cell)
pattern NativeDef arity impl = ValNativeFn (LoxNativeFn arity (NativeFnImpl impl))

data LoxNativeFn = LoxNativeFn { loxNativeFnArity :: Int
                               , loxNativeFnNameImpl :: NativeFnImpl
                               }
  deriving (Show)

type NativeFnImplFn cell sig m = Has NativeFns sig m => Token -> Seq (RTValue cell) -> m (RTValue cell)

newtype NativeFnImpl
  = NativeFnImpl { runNativeFnImpl :: forall cell sig m. NativeFnImplFn cell sig m }

instance Show NativeFnImpl where
  show _ = "<NativeFnImpl>"
