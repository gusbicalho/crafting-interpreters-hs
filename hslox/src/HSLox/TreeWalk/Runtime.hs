{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module HSLox.TreeWalk.Runtime (
  Trace,
  Trace.trace,
  module HSLox.TreeWalk.Runtime,
) where

import Control.Algebra (Has)
import Control.Effect.Error (Error)
import Control.Effect.State (State)
import Control.Effect.Trace (Trace)
import Control.Effect.Trace qualified as Trace
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import Data.Text qualified as T
import HSLox.Cells.Effect (Cells)
import HSLox.NativeFns.Effect (NativeFns)
import HSLox.Token (Token (..))
import Data.Text (Text)
import Data.Proxy (Proxy(Proxy))

type Runtime cell sig m =
  ( Has NativeFns sig m
  , Has (Cells cell) sig m
  , Has (Error RTError) sig m
  , Has (Error (RTReturn cell)) sig m
  , Has (State (RTState cell)) sig m
  )

type BindingName = T.Text

newtype RTCell cell = RTCell {unRTCell :: cell (RTValue cell)}

newtype RTEnv cell = RTEnv {rtEnvBindings :: Map BindingName (RTCell cell)}

data RTFrame cell = RTFrame
  { rtFrameEnv :: RTEnv cell
  , rtFrameEnclosing :: Maybe (RTFrame cell)
  }

instance Show (RTFrame cell) where
  show _ = "<stack frame>"

data RTState cell = RTState
  { rtStateGlobalEnv :: RTEnv cell
  , rtStateLocalFrame :: Maybe (RTFrame cell)
  }

data RTReturn cell = RTReturn Token (RTValue cell)

data RTError = RTError
  { rtErrorMessage :: T.Text
  , rtErrorToken :: Token
  }
  deriving stock (Eq, Show)

data RTValue cell
  = ValString T.Text
  | ValNum Double
  | ValBool Bool
  | ValNil
  | ValFn (LoxFn cell)
  | ValClass (LoxClass cell)
  | ValInstance (LoxInstance cell)
  | ValNativeFn LoxNativeFn

type RuntimeAction :: (Type -> Type) -> Type -> Type
newtype RuntimeAction cell a = RuntimeAction (forall mRun sig. Runtime cell sig mRun => mRun a)

runAction :: forall cell mRun sig a. Runtime cell sig mRun => RuntimeAction cell a -> mRun a
runAction (RuntimeAction action) = action

inRuntime ::
  forall cell mBuild a.
  Applicative mBuild =>
  (forall mRun sig proxy. Runtime cell sig mRun => proxy cell -> mRun a) ->
  mBuild (RuntimeAction cell a)
inRuntime action = pure $ RuntimeAction (action (Proxy @cell))

data LoxFn cell where
  LoxFn ::
    { loxClosedEnv :: Maybe (RTFrame cell)
    , loxFnIdentifier :: Maybe Text
    , loxFnIsInitializer :: Bool
    , loxFnArity :: Int
    , loxFnAction ::
      Maybe (RTFrame cell) ->
      RTValue cell ->
      Seq (RTValue cell) ->
      RuntimeAction cell (RTValue cell)
    } ->
    LoxFn cell

data LoxClass (cell :: Type -> Type) = LoxClass
  { loxClassName :: T.Text
  , loxClassSuperclass :: Maybe (LoxClass cell)
  , loxClassMethodTable :: Map T.Text (LoxFn cell)
  }

data LoxInstance cell = LoxInstance
  { loxInstanceClass :: LoxClass cell
  , loxInstanceState :: cell (Map T.Text (RTValue cell))
  }

pattern NativeDef ::
  Int ->
  (forall cell sig m. NativeFnImplFn cell sig m) ->
  RTValue cell
pattern NativeDef arity impl = ValNativeFn (LoxNativeFn arity (NativeFnImpl impl))

data LoxNativeFn = LoxNativeFn
  { loxNativeFnArity :: Int
  , loxNativeFnNameImpl :: NativeFnImpl
  }
  deriving stock (Show)

type NativeFnImplFn cell sig m = Has NativeFns sig m => Token -> Seq (RTValue cell) -> m (RTValue cell)

newtype NativeFnImpl = NativeFnImpl {runNativeFnImpl :: forall cell sig m. NativeFnImplFn cell sig m}

instance Show NativeFnImpl where
  show _ = "<NativeFnImpl>"
