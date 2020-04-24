module HSLox.Cells.Effect
  ( Cells (..)
  , newCell, readCell, writeCell
  -- * Re-exports
  , Algebra, Has, run
  ) where

import Control.Algebra
import Data.Kind

data Cells cellType (m :: Type -> Type) k where
  NewCell :: val -> Cells cellType m (cellType val)
  ReadCell :: cellType val -> Cells cellType m val
  WriteCell :: val -> cellType val -> Cells cellType m ()

newCell :: Has (Cells cellType) sig m => val -> m (cellType val)
newCell initialVal = send (NewCell initialVal)

readCell :: Has (Cells cellType) sig m => cellType val -> m val
readCell cell = send (ReadCell cell)

writeCell :: Has (Cells cellType) sig m => val -> cellType val -> m ()
writeCell val cell = send (WriteCell val cell)
