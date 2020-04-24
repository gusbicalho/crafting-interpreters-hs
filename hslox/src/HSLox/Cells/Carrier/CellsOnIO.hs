{-# LANGUAGE UndecidableInstances #-}
module HSLox.Cells.Carrier.CellsOnIO
  ( CellsOnIOC, runCellsOnIO, Cell
  -- * Re-exports
  , module HSLox.Cells.Effect
  ) where

import Control.Algebra
import Control.Effect.Lift
import Data.Functor
import HSLox.Cells.Effect
import qualified Data.IORef as IORef

type Cell = IORef.IORef

newtype CellsOnIOC m a = CellsOnIOC { runCellsOnIO :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Has (Lift IO) sig m
         => Algebra (Cells Cell :+: sig) (CellsOnIOC m) where
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
