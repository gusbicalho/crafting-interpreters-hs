{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE StrictData #-}
module HSLox.TreeWalk.Runtime
  ( Trace, trace
  , module HSLox.TreeWalk.Runtime
  ) where

import Control.Effect.Trace
import Control.Effect.Error
import Control.Effect.State
import Data.Kind
import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import qualified Data.Text as T
import qualified HSLox.AST as AST
import qualified HSLox.AST.Meta as AST.Meta
import qualified HSLox.AST.WalkAST as WalkAST
import HSLox.Cells.Effect
import HSLox.NativeFns.Effect
import qualified HSLox.StaticAnalysis.Analyzer as Analyzer
import HSLox.Token (Token (..))

type Runtime cell sig m = ( Has NativeFns sig m
                          , Has (Cells cell) sig m
                          , Has (Error RTError) sig m
                          , Has (Error (RTReturn cell)) sig m
                          , Has (State (RTState cell)) sig m
                          )

type BindingName = T.Text

newtype RTCell cell = RTCell { unRTCell :: cell (RTValue cell) }

newtype RTEnv cell = RTEnv { rtEnvBindings :: Map BindingName (RTCell cell) }

data RTFrame cell = RTFrame { rtFrameEnv :: RTEnv cell
                            , rtFrameEnclosing :: Maybe (RTFrame cell)
                            }

instance Show (RTFrame cell) where
  show _ = "<stack frame>"

data RTState cell = RTState { rtStateGlobalEnv :: RTEnv cell
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
  | ValClass (LoxClass cell)
  | ValInstance (LoxInstance cell)
  | ValNativeFn LoxNativeFn

newtype RuntimeAST a = RuntimeAST (AST.Meta.WithMeta Analyzer.ResolverMeta
                                    AST.Meta.Identity
                                    a)
  deriving (Show, Functor, Foldable, Traversable)
instance AST.Meta.AsIdentity RuntimeAST where
  asIdentity (RuntimeAST fa) = AST.Meta.asIdentity fa
instance AST.Meta.HasMeta Analyzer.ResolverMeta RuntimeAST where
  meta (RuntimeAST fa) = AST.Meta.meta fa

asRuntimeAST :: Monad m
             => Traversable f
             => AST.Meta.AsIdentity f
             => AST.Meta.HasMeta Analyzer.ResolverMeta f
             => WalkAST.WalkAST astNode
             => astNode f -> m (astNode RuntimeAST)
asRuntimeAST ast = WalkAST.walkAST preWalk pure ast
  where
    preWalk fa = do
      pure $ RuntimeAST
           . AST.Meta.withMeta (AST.Meta.meta @Analyzer.ResolverMeta fa)
           . AST.Meta.asIdentity
           $ fa

data LoxFn cell = LoxFn { loxFnAST :: AST.Function RuntimeAST
                        , loxClosedEnv :: Maybe (RTFrame cell)
                        }

data LoxClass (cell :: Type -> Type) =
  LoxClass { loxClassName :: T.Text
           , loxClassMethodTable :: Map T.Text (LoxFn cell)
           }

data LoxInstance cell =
  LoxInstance { loxInstanceClass :: LoxClass cell
              , loxInstanceState :: (cell (Map T.Text (RTValue cell)))}

pattern NativeDef :: Int
                  -> (forall cell sig m. NativeFnImplFn cell sig m)
                  -> RTValue cell
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
