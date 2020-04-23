{-# LANGUAGE UndecidableInstances #-}
module HSLox.NativeFns.Carrier.NativeFnsOnIO where

import Control.Algebra
import Control.Carrier.Lift
import Data.Functor
import qualified Data.IORef as IORef
import qualified Data.Text.IO as T.IO
import HSLox.NativeFns.Effect
import qualified System.Clock as SysClock

newtype NativeFnsOnIOC m a = NativeFnsOnIOC { runNativeFnsOnIO :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Has (Lift IO) sig m
         => Algebra (NativeFns :+: sig) (NativeFnsOnIOC m) where
  alg hdl sig ctx = NativeFnsOnIOC $ case sig of
    L Clock -> do
      secs <- sendM @IO getSystemMonotonicClockMillis
      pure (secs <$ ctx)
    L (PrintText t) -> do
      sendM @IO $ T.IO.putStrLn t
      pure ctx
    R other -> alg (runNativeFnsOnIO . hdl) other ctx
  {-# INLINE alg #-}

getSystemMonotonicClockMillis :: IO Integer
getSystemMonotonicClockMillis =
  SysClock.getTime SysClock.Monotonic
    <&> SysClock.toNanoSecs
    <&> (`div` 1000000)

newtype CellsOnIOC m a = CellsOnIOC { runCellsOnIO :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Has (Lift IO) sig m
         => Algebra (Cells IORef.IORef :+: sig) (CellsOnIOC m) where
  alg hdl sig ctx = CellsOnIOC $ case sig of
    L (NewCell initialVal) -> do
      ref <- sendM @IO $ IORef.newIORef initialVal
      pure $ ctx $> ref
    L (ReadCell ref) -> do
      val <- sendM @IO $ IORef.readIORef ref
      pure $ ctx $> val
    L (WriteCell val ref) -> do
      sendM @IO $ IORef.writeIORef ref val
      pure ctx
    R other -> alg (runCellsOnIO . hdl) other ctx
  {-# INLINE alg #-}
