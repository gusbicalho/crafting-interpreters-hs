{-# LANGUAGE UndecidableInstances #-}

module HSLox.Cells.Carrier.CellsOnST (
  CellsOnSTC,
  runCellsOnST,
  Cell,

  -- * Re-exports
  ST.ST,
  ST.runST,
  module HSLox.Cells.Effect,
) where

import Control.Algebra (Algebra (..), Has, type (:+:) (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Lift qualified as Lift
import Control.Monad.ST qualified as ST
import Data.Functor (($>))
import Data.STRef.Strict qualified as STRef
import HSLox.Cells.Effect (Cells)
import HSLox.Cells.Effect qualified as Cells

type Cell s = STRef.STRef s

newtype CellsOnSTC s m a = CellsOnSTC {runCellsOnST :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance
  Has (Lift (ST.ST s)) sig m =>
  Algebra (Cells (Cell s) :+: sig) (CellsOnSTC s m)
  where
  alg hdl sig ctx = CellsOnSTC $ case sig of
    L (Cells.NewCell initialVal) -> do
      ref <- Lift.sendM @(ST.ST s) $ STRef.newSTRef initialVal
      pure $ ctx $> ref
    L (Cells.ReadCell ref) -> do
      val <- Lift.sendM @(ST.ST s) $ STRef.readSTRef ref
      pure $ ctx $> val
    L (Cells.WriteCell val ref) -> do
      Lift.sendM @(ST.ST s) $ STRef.writeSTRef ref val
      pure ctx
    R other -> alg (runCellsOnST . hdl) other ctx
  {-# INLINE alg #-}
