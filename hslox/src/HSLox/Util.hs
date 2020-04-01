module HSLox.Util where

import Control.Carrier.Empty.Church
import Control.Monad (forever)

runEmptyToMaybe :: Applicative m => EmptyC m a -> m (Maybe a)
runEmptyToMaybe = runEmpty (pure Nothing) (pure . Just)

runEmptyToUnit :: Applicative m => EmptyC m a -> m ()
runEmptyToUnit = runEmpty (pure ()) (const $ pure ())

runEmptyToBool :: Applicative m => EmptyC m a -> m Bool
runEmptyToBool = runEmpty (pure False) (const $ pure True)

untilEmpty :: Applicative m => EmptyC m a -> m ()
untilEmpty = runEmptyToUnit . forever

anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM predicate = foldr (orNext . predicate) (pure False)
  where
    ma `orNext` mb = do
      a <- ma
      if a then pure True else mb

allM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM predicate = foldr (andNext . predicate) (pure True)
  where
    ma `andNext` mb = do
      a <- ma
      if a then mb else pure False
