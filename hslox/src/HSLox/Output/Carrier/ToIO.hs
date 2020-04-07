{-# LANGUAGE UndecidableInstances #-}
module HSLox.Output.Carrier.ToIO where

import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.Lift
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import HSLox.Output.Effect
import System.IO (Handle)

newtype OutputToIOC o m a = OutputToIOC { runOutputToIOC :: ReaderC (o -> IO ()) m a }
  deriving newtype (Functor, Applicative, Monad)

runOutputToIO :: (o -> IO ()) -> OutputToIOC o m a -> m a
runOutputToIO onOutput = runReader onOutput . runOutputToIOC

runOutputText :: Handle -> OutputToIOC T.Text m a -> m a
runOutputText handle = runOutputToIO (T.IO.hPutStrLn handle)

instance Has (Lift IO) sig m
         => Algebra (Output o :+: sig) (OutputToIOC o m) where
  alg hdl sig ctx = OutputToIOC $ case sig of
    L (Output o) -> do
      onOutput <- ask @(o -> IO ())
      sendM @IO (onOutput o)
      pure ctx
    R other -> alg (runOutputToIOC . hdl) (R other) ctx
  {-# INLINE alg #-}
