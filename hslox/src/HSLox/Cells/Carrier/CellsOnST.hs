{-# LANGUAGE UndecidableInstances #-}
module HSLox.Cells.Carrier.CellsOnST
  ( CellsOnSTC, runCellsOnST, Cell
  -- * Re-exports
  , ST.ST, ST.runST
  , module HSLox.Cells.Effect
  ) where

import Control.Algebra
import Control.Effect.Lift
import qualified Control.Monad.ST as ST
import Data.Functor
import HSLox.Cells.Effect
import qualified Data.STRef.Strict as STRef

type Cell s = STRef.STRef s

newtype CellsOnSTC s m a = CellsOnSTC { runCellsOnST :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Has (Lift (ST.ST s)) sig m
         => Algebra (Cells (Cell s) :+: sig) (CellsOnSTC s m) where
  alg hdl sig ctx = CellsOnSTC $ case sig of
    L (NewCell initialVal) -> do
      ref <- sendM @(ST.ST s) $ STRef.newSTRef initialVal
      pure $ ctx $> ref
    L (ReadCell ref) -> do
      val <- sendM @(ST.ST s) $ STRef.readSTRef ref
      pure $ ctx $> val
    L (WriteCell val ref) -> do
      sendM @(ST.ST s) $ STRef.writeSTRef ref val
      pure ctx
    R other -> alg (runCellsOnST . hdl) other ctx
  {-# INLINE alg #-}
