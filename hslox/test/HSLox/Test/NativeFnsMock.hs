{-# LANGUAGE UndecidableInstances #-}

module HSLox.Test.NativeFnsMock where

import Control.Algebra (Algebra (..), Has, type (:+:) (..))
import Control.Carrier.State.Strict qualified as State.Strict
import Control.Effect.State qualified as State
import Control.Effect.Writer (Writer)
import Control.Effect.Writer qualified as Writer
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import HSLox.NativeFns.Effect (NativeFns)
import HSLox.NativeFns.Effect qualified as NativeFns

newtype NativeFnsMockC m a = NativeFnsMockC
  { runNativeFnsMockC :: State.Strict.StateC Integer m a
  }
  deriving newtype (Functor, Applicative, Monad)

runNativeFnsMock :: Functor m => NativeFnsMockC m a -> m a
runNativeFnsMock = State.Strict.evalState 0 . runNativeFnsMockC

instance
  (Has (Writer (Seq.Seq T.Text)) sig m) =>
  Algebra (NativeFns :+: sig) (NativeFnsMockC m)
  where
  alg hdl sig ctx = NativeFnsMockC $ case sig of
    L NativeFns.Clock -> do
      t <- State.get @Integer
      State.modify @Integer succ
      pure (t <$ ctx)
    L (NativeFns.PrintText text) -> do
      Writer.tell $ Seq.singleton text
      pure ctx
    R other -> alg (runNativeFnsMockC . hdl) (R other) ctx
  {-# INLINE alg #-}
