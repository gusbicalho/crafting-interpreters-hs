module HSLox.NativeFns.Effect
  ( Algebra, Has
  , module HSLox.NativeFns.Effect
  ) where

import Control.Algebra
import Data.Kind
import Data.Int (Int64)
import qualified Data.Text as T

data NativeFns (m :: Type -> Type) k where
  Clock :: NativeFns m Int64
  PrintText :: T.Text -> NativeFns m ()

clock :: Has NativeFns sig m => m Int64
clock = send Clock

printText :: Has NativeFns sig m => T.Text -> m ()
printText test = send (PrintText test)
