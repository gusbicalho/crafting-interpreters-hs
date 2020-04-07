{-# LANGUAGE UndecidableInstances #-}
module HSLox.Output.Carrier.Transform where

import Control.Algebra
import Control.Carrier.Reader
import HSLox.Output.Effect

newtype OutputTransformC to from m a = OutputTransformC { runOutputTransformC :: ReaderC (from -> to) m a }
  deriving newtype (Functor, Applicative, Monad)

runOutputTransform :: (from -> to) -> OutputTransformC to from m a -> m a
runOutputTransform onOutput = runReader onOutput . runOutputTransformC

instance Has (Output to) sig m
         => Algebra (Output from :+: sig) (OutputTransformC to from m) where
  alg hdl sig ctx = OutputTransformC $ case sig of
    L (Output o) -> do
      transform <- ask @(from -> to)
      output (transform o)
      pure ctx
    R other -> alg (runOutputTransformC . hdl) (R other) ctx
  {-# INLINE alg #-}
