module HSLox.Output.Effect where

import Control.Algebra
import Data.Kind

data Output o (m :: Type -> Type) k where
  Output :: o -> Output o m ()

output :: (Has (Output o) sig m) => o -> m ()
output o = send (Output o)
