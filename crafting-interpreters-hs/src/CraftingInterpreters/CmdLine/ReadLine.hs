{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module CraftingInterpreters.CmdLine.ReadLine
  ( ReadLine (..), readLine
  , ReadLineC, runReadLine
  -- * Re-exports
  , Has
  ) where

import Control.Algebra
import Control.Effect.Lift
import Data.Text
import GHC.Generics (Generic1)

data ReadLine m k
  = ReadLine (Text -> m k)
  deriving (Functor, Generic1, HFunctor, Effect)

readLine :: (Has ReadLine sig m) => m Text
readLine = send $ ReadLine pure

newtype ReadLineC m a = ReadLineC {
  runReadLine :: m a
  } deriving newtype (Functor, Applicative, Monad)

instance ( Has (Lift IO) sig m
         ) => Algebra (ReadLine :+: sig) (ReadLineC m) where
  alg (L (ReadLine k)) = k =<< ReadLineC do
    sendM @IO $ do
      putStr "> "
      pack <$> getLine
  alg (R other) = ReadLineC (alg (handleCoercible other))
