{-# LANGUAGE UndecidableInstances #-}
module HSLox.Test.MockClock where

import Control.Algebra
import Control.Carrier.State.Strict
import Data.Int (Int64)
import HSLox.MonotonicClock.Effect

newtype IncClockC m a = IncClockC { runIncClockC :: StateC Int64 m a }
  deriving newtype (Functor, Applicative, Monad)

runIncClock :: Functor m => Int64 -> IncClockC m a -> m a
runIncClock initialSecs = evalState initialSecs . runIncClockC

instance Algebra sig m => Algebra (MonotonicClock :+: sig) (IncClockC m) where
  alg hdl sig ctx = IncClockC $ case sig of
    L GetSeconds -> do
      t <- get @Int64
      modify @Int64 succ
      pure (t <$ ctx)
    R other -> alg (runIncClockC . hdl) (R other) ctx
  {-# INLINE alg #-}
