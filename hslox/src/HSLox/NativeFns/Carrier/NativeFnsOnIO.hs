{-# LANGUAGE UndecidableInstances #-}
module HSLox.NativeFns.Carrier.NativeFnsOnIO where

import Control.Algebra
import Control.Carrier.Lift
import Data.Int (Int64)
import qualified Data.Text.IO as T.IO
import HSLox.NativeFns.Effect
import qualified System.Clock as SysClock

newtype NativeFnsOnIOC m a = NativeFnsOnIOC { runNativeFnsOnIO :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Has (Lift IO) sig m
         => Algebra (NativeFns :+: sig) (NativeFnsOnIOC m) where
  alg hdl sig ctx = NativeFnsOnIOC $ case sig of
    L Clock -> do
      secs <- sendM @IO getSystemMonotonicClockSeconds
      pure (secs <$ ctx)
    L (PrintText t) -> do
      sendM @IO $ T.IO.putStrLn t
      pure ctx
    R other -> alg (runNativeFnsOnIO . hdl) other ctx
  {-# INLINE alg #-}

getSystemMonotonicClockSeconds :: IO Int64
getSystemMonotonicClockSeconds =
  SysClock.sec <$> SysClock.getTime SysClock.Monotonic
