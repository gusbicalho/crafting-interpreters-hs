{-# LANGUAGE AllowAmbiguousTypes #-}
module HSLox.Util where

import Control.Carrier.Empty.Church
import Control.Carrier.Error.Church
import Control.Effect.State
import Control.Effect.Trace
import Control.Monad (forever)

runEmptyToMaybe :: Applicative m => EmptyC m a -> m (Maybe a)
runEmptyToMaybe = runEmpty (pure Nothing) (pure . Just)

runEmptyToUnit :: Applicative m => EmptyC m a -> m ()
runEmptyToUnit = runEmpty (pure ()) (const $ pure ())

runEmptyToBool :: Applicative m => EmptyC m a -> m Bool
runEmptyToBool = runEmpty (pure False) (const $ pure True)

recoverFromEmptyWith :: Applicative m => EmptyC m a -> m a -> m a
recoverFromEmptyWith ma recover = runEmpty recover pure ma

untilEmpty :: Applicative m => EmptyC m a -> m ()
untilEmpty = runEmptyToUnit . forever

runErrorToEither :: forall e m a. Applicative m => ErrorC e m a -> m (Either e a)
runErrorToEither = runError (pure . Left) (pure . Right)

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

restoringState :: forall s a sig m. Has (State s) sig m => m a -> m a
restoringState action = do
  s <- get @s
  result <- action
  put @s s
  pure result

tap :: (Has Trace sig m, Show a) => a -> m a
tap a = trace (show a) >> pure a

tapM :: (Has Trace sig m, Show a) => m a -> m a
tapM ma = do
  a <- ma
  trace (show a)
  pure a
