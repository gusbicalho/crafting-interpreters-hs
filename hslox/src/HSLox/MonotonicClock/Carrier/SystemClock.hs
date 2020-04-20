{-# LANGUAGE UndecidableInstances #-}
module HSLox.MonotonicClock.Carrier.SystemClock where

import Control.Algebra
import Control.Carrier.Lift
import Data.Int (Int64)
import HSLox.MonotonicClock.Effect
import qualified System.Clock as SysClock

newtype MonotonicSystemClockC m a = MonotonicSystemClockC { runMonotonicSystemClock :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Has (Lift IO) sig m
         => Algebra (MonotonicClock :+: sig) (MonotonicSystemClockC m) where
  alg hdl sig ctx = MonotonicSystemClockC $ case sig of
    L GetSeconds -> do
      secs <- sendM @IO getMonotonicSystemClockSeconds
      pure (secs <$ ctx)
    R other -> alg (runMonotonicSystemClock . hdl) other ctx
  {-# INLINE alg #-}

getMonotonicSystemClockSeconds :: IO Int64
getMonotonicSystemClockSeconds =
  SysClock.sec <$> SysClock.getTime SysClock.Monotonic
