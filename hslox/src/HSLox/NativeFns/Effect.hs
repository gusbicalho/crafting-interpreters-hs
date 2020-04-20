module HSLox.NativeFns.Effect
  ( Algebra, Has
  , module HSLox.NativeFns.Effect
  ) where

import Control.Algebra
import Data.Kind
import Data.Int (Int64)

data NativeFns (m :: Type -> Type) k where
  Clock :: NativeFns m Int64

clock :: Has NativeFns sig m => m Int64
clock = send Clock
