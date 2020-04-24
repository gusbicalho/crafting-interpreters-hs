{-# LANGUAGE UndecidableInstances #-}
module HSLox.Test.NativeFnsMock
  ( ST.ST, ST.runST
  , module HSLox.Test.NativeFnsMock
  ) where

import Control.Algebra
import Control.Effect.Writer
import Control.Effect.Lift
import Control.Carrier.State.Strict
import qualified Control.Monad.ST as ST
import Data.Functor
import Data.STRef.Strict (STRef)
import qualified Data.STRef.Strict as STRef
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import HSLox.NativeFns.Effect

newtype NativeFnsMockC m a =
  NativeFnsMockC {
    runNativeFnsMockC :: StateC Integer m a
  }
  deriving newtype (Functor, Applicative, Monad)

runNativeFnsMock :: Functor m => NativeFnsMockC m a -> m a
runNativeFnsMock = evalState 0 . runNativeFnsMockC

instance ( Has (Writer (Seq.Seq T.Text)) sig m )
         => Algebra (NativeFns :+: sig) (NativeFnsMockC m) where
  alg hdl sig ctx = NativeFnsMockC $ case sig of
    L Clock -> do
      t <- get @Integer
      modify @Integer succ
      pure (t <$ ctx)
    L (PrintText text) -> do
      tell $ Seq.singleton text
      pure ctx
    R other -> alg (runNativeFnsMockC . hdl) (R other) ctx
  {-# INLINE alg #-}

type Cell s = STRef s

newtype CellsOnSTC s m a = CellsOnSTC { runCellsOnST :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Has (Lift (ST.ST s)) sig m
         => Algebra (Cells (STRef s) :+: sig) (CellsOnSTC s m) where
  alg hdl sig ctx = CellsOnSTC $ case sig of
    L (NewCell initialVal) -> do
      ref <- sendM @(ST.ST s) $ STRef.newSTRef initialVal
      pure $ ctx $> ref
    L (ReadCell ref) -> do
      val <- sendM @(ST.ST s) $ STRef.readSTRef ref
      pure $ ctx $> val
    L (WriteCell val ref) -> do
      sendM @(ST.ST s) $ STRef.writeSTRef ref val
      pure ctx
    R other -> alg (runCellsOnST . hdl) other ctx
  {-# INLINE alg #-}
