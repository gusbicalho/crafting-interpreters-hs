{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module HSLox.CmdLine.ReadLine (
  ReadLine (..),
  readLine,
  ReadLineC,
  runReadLine,

  -- * Re-exports
  Has,
) where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Control.Carrier.Lift (Lift)
import Control.Carrier.Lift qualified as Lift
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import System.IO (hFlush, stdout)

data ReadLine (m :: Type -> Type) k where
  ReadLine :: ReadLine m Text

readLine :: (Has ReadLine sig m) => m Text
readLine = send ReadLine

newtype ReadLineC m a = ReadLineC
  { runReadLine :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

instance
  Has (Lift IO) sig m =>
  Algebra (ReadLine :+: sig) (ReadLineC m)
  where
  alg hdl sig ctx = case sig of
    L ReadLine -> fmap (<$ ctx) $
      Lift.sendM @IO $ do
        putStr "> "
        hFlush stdout
        T.pack <$> getLine
    R other -> ReadLineC (alg (coerce . hdl) other ctx)
