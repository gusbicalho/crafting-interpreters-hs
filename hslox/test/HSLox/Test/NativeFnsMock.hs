{-# LANGUAGE UndecidableInstances #-}
module HSLox.Test.NativeFnsMock where

import Control.Algebra
import Control.Effect.Writer
import Control.Carrier.State.Strict
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import HSLox.NativeFns.Effect

newtype NativeFnsMockC m a =
  NativeFnsMockC {
    runNativeFnsMockC :: StateC Integer m a
  }
  deriving newtype (Functor, Applicative, Monad)

runNativeFnsMock :: Functor m => NativeFnsMockC m a -> m a
runNativeFnsMock = evalState 0 . runNativeFnsMockC

instance ( Has (Writer (Seq.Seq T.Text)) sig m )
         => Algebra (NativeFns :+: sig) (NativeFnsMockC m) where
  alg hdl sig ctx = NativeFnsMockC $ case sig of
    L Clock -> do
      t <- get @Integer
      modify @Integer succ
      pure (t <$ ctx)
    L (PrintText text) -> do
      tell $ Seq.singleton text
      pure ctx
    R other -> alg (runNativeFnsMockC . hdl) (R other) ctx
  {-# INLINE alg #-}
