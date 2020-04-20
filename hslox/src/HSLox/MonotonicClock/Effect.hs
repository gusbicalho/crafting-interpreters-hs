module HSLox.MonotonicClock.Effect
  ( Algebra, Has
  , module HSLox.MonotonicClock.Effect
  ) where

import Control.Algebra
import Data.Kind
import Data.Int (Int64)

data MonotonicClock (m :: Type -> Type) k where
  GetSeconds :: MonotonicClock m Int64

getSeconds :: Has MonotonicClock sig m => m Int64
getSeconds = send GetSeconds
