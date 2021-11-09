{-# LANGUAGE UndecidableInstances #-}

module HSLox.NativeFns.Carrier.NativeFnsOnIO where

import Control.Algebra (Algebra (..), Has, type (:+:) (..))
import Control.Carrier.Lift (Lift)
import Control.Carrier.Lift qualified as Lift
import Data.Functor ((<&>))
import Data.Text.IO qualified as T.IO
import HSLox.NativeFns.Effect (NativeFns)
import HSLox.NativeFns.Effect qualified as NativeFns
import System.Clock qualified as SysClock

newtype NativeFnsOnIOC m a = NativeFnsOnIOC {runNativeFnsOnIO :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance
  Has (Lift IO) sig m =>
  Algebra (NativeFns :+: sig) (NativeFnsOnIOC m)
  where
  alg hdl sig ctx = NativeFnsOnIOC $ case sig of
    L NativeFns.Clock -> do
      secs <- Lift.sendM @IO getSystemMonotonicClockMillis
      pure (secs <$ ctx)
    L (NativeFns.PrintText t) -> do
      Lift.sendM @IO $ T.IO.putStrLn t
      pure ctx
    R other -> alg (runNativeFnsOnIO . hdl) other ctx
  {-# INLINE alg #-}

getSystemMonotonicClockMillis :: IO Integer
getSystemMonotonicClockMillis =
  SysClock.getTime SysClock.Monotonic
    <&> SysClock.toNanoSecs
    <&> (`div` 1000000)
