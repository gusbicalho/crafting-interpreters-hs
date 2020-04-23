module HSLox.NativeFns.Effect
  ( Algebra, Has
  , module HSLox.NativeFns.Effect
  ) where

import Control.Algebra
import Data.Kind
import qualified Data.Text as T

data NativeFns (m :: Type -> Type) k where
  Clock :: NativeFns m Integer
  PrintText :: T.Text -> NativeFns m ()

data Cells cellType (m :: Type -> Type) k where
  NewCell :: val -> Cells cellType m (cellType val)
  ReadCell :: cellType val -> Cells cellType m val
  WriteCell :: val -> cellType val -> Cells cellType m ()

clock :: Has NativeFns sig m => m Integer
clock = send Clock

printText :: Has NativeFns sig m => T.Text -> m ()
printText test = send (PrintText test)

newCell :: Has (Cells cellType) sig m => val -> m (cellType val)
newCell initialVal = send (NewCell initialVal)

readCell :: Has (Cells cellType) sig m => cellType val -> m val
readCell cell = send (ReadCell cell)

writeCell :: Has (Cells cellType) sig m => val -> cellType val -> m ()
writeCell val cell = send (WriteCell val cell)
