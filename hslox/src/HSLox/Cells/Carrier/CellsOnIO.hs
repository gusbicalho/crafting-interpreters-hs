{-# LANGUAGE UndecidableInstances #-}

module HSLox.Cells.Carrier.CellsOnIO (
  CellsOnIOC,
  runCellsOnIO,
  Cell,

  -- * Re-exports
  module HSLox.Cells.Effect,
) where

import Control.Algebra (Algebra (..), Has, type (:+:) (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Lift qualified as Lift
import Data.Functor (($>))
import Data.IORef qualified as IORef
import HSLox.Cells.Effect (Cells)
import HSLox.Cells.Effect qualified as Cells

type Cell = IORef.IORef

newtype CellsOnIOC m a = CellsOnIOC {runCellsOnIO :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance
  Has (Lift IO) sig m =>
  Algebra (Cells Cell :+: sig) (CellsOnIOC m)
  where
  alg hdl sig ctx = CellsOnIOC $ case sig of
    L (Cells.NewCell initialVal) -> do
      ref <- Lift.sendM @IO $ IORef.newIORef initialVal
      pure $ ctx $> ref
    L (Cells.ReadCell ref) -> do
      val <- Lift.sendM @IO $ IORef.readIORef ref
      pure $ ctx $> val
    L (Cells.WriteCell val ref) -> do
      Lift.sendM @IO $ IORef.writeIORef ref val
      pure ctx
    R other -> alg (runCellsOnIO . hdl) other ctx
  {-# INLINE alg #-}
