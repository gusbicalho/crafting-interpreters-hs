{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module HSLox.CmdLine.ReadLine
  ( ReadLine (..), readLine
  , ReadLineC, runReadLine
  -- * Re-exports
  , Has
  ) where

import Control.Algebra
import Control.Carrier.Lift
import Data.Text
import Data.Kind
import System.IO (hGetLine, hFlush, hPutStr, stdin, stdout)

data ReadLine (m :: Type -> Type) k where
  ReadLine :: ReadLine m Text

readLine :: (Has ReadLine sig m) => m Text
readLine = send $ ReadLine

newtype ReadLineC m a = ReadLineC {
  runReadLine :: m a
  } deriving newtype (Functor, Applicative, Monad)

instance ( Has (Lift IO) sig m
         ) => Algebra (ReadLine :+: sig) (ReadLineC m) where
  alg hdl sig ctx = case sig of
    L ReadLine -> fmap (<$ ctx) $
      sendM @IO $ do
        hPutStr stdout "> "
        hFlush stdout
        pack <$> hGetLine stdin
    R other -> ReadLineC (alg (runReadLine . hdl) other ctx)
