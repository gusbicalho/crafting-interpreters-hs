{-# LANGUAGE UndecidableInstances #-}
module HSLox.Output.Carrier.ToWriter where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Writer
import HSLox.Output.Effect

newtype OutputToWriterC w o m a = OutputToWriterC { runOutputToWriterC :: ReaderC (o -> w) m a }
  deriving newtype (Functor, Applicative, Monad)

runOutputToWriter :: (o -> w) -> OutputToWriterC w o m a -> m a
runOutputToWriter toW = runReader toW . runOutputToWriterC

instance Has (Writer w) sig m
         => Algebra (Output o :+: sig) (OutputToWriterC w o m) where
  alg hdl sig ctx = OutputToWriterC $ case sig of
    L (Output o) -> do
      transform <- ask @(o -> w)
      tell (transform o)
      pure ctx
    R other -> alg (runOutputToWriterC . hdl) (R other) ctx
  {-# INLINE alg #-}
