{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module HSLox.CmdLine.ReadLine
  ( ReadLine (..), readLine
  , ReadLineC, runReadLine
  -- * Re-exports
  , Has
  ) where

import Control.Algebra
import Control.Monad.IO.Class
import Data.Text
import GHC.Generics (Generic1)
import System.IO (hGetLine, hFlush, hPutStr, stdin, stdout)

data ReadLine m k
  = ReadLine (Text -> m k)
  deriving (Functor, Generic1, HFunctor, Effect)

readLine :: (Has ReadLine sig m) => m Text
readLine = send $ ReadLine pure

newtype ReadLineC m a = ReadLineC {
  runReadLine :: m a
  } deriving newtype (Functor, Applicative, Monad)

instance ( MonadIO m, Algebra sig m
         ) => Algebra (ReadLine :+: sig) (ReadLineC m) where
  alg (L (ReadLine k)) = k =<< ReadLineC do
    liftIO $ do
      hPutStr stdout "> "
      hFlush stdout
      pack <$> hGetLine stdin
  alg (R other) = ReadLineC (alg (handleCoercible other))
