-- |
-- Module      :  HSLox.Util
-- Copyright   :  © 2020-Gustavo Bicalho
--
-- Useful fns.

{-# LANGUAGE AllowAmbiguousTypes #-}
module HSLox.Util where

import Control.Carrier.Empty.Church
import Control.Carrier.Error.Church
import Control.Carrier.State.Church
import Control.Carrier.Writer.Church
import Control.Effect.Trace
import Control.Monad (forever)
import Data.Foldable

runEmptyToMaybe :: Applicative m => EmptyC m a -> m (Maybe a)
runEmptyToMaybe = runEmpty (pure Nothing) (pure . Just)
{-# INLINE runEmptyToMaybe #-}

runEmptyToUnit :: Applicative m => EmptyC m a -> m ()
runEmptyToUnit = runEmpty (pure ()) (const $ pure ())
{-# INLINE runEmptyToUnit #-}

runEmptyToBool :: Applicative m => EmptyC m a -> m Bool
runEmptyToBool = runEmpty (pure False) (const $ pure True)
{-# INLINE runEmptyToBool #-}

recoverFromEmptyWith :: Applicative m => EmptyC m a -> m a -> m a
recoverFromEmptyWith ma recover = runEmpty recover pure ma
{-# INLINE recoverFromEmptyWith #-}

untilEmpty :: Applicative m => EmptyC m a -> m ()
untilEmpty = runEmptyToUnit . forever
{-# INLINE untilEmpty #-}

runErrorToEither :: forall e m a. Applicative m => ErrorC e m a -> m (Either e a)
runErrorToEither = runError (pure . Left) (pure . Right)
{-# INLINE runErrorToEither #-}

runErrorToUnit :: forall e m a. Applicative m => ErrorC e m a -> m ()
runErrorToUnit = runError (const $ pure ()) (const $ pure ())
{-# INLINE runErrorToUnit #-}

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
{-# INLINE restoringState #-}

tap :: (Has Trace sig m, Show a) => a -> m a
tap a = trace (show a) >> pure a

tapM :: (Has Trace sig m, Show a) => m a -> m a
tapM ma = do
  a <- ma
  trace (show a)
  pure a

runStateToPair :: forall s m a. Applicative m => s -> StateC s m a -> m (s, a)
runStateToPair = runState (\s a -> pure (s, a))
{-# INLINE runStateToPair #-}

runWriterToPair :: forall w m a. (Monoid w, Applicative m) => WriterC w m a -> m (w, a)
runWriterToPair = runWriter (\w a -> pure (w, a))
{-# INLINE runWriterToPair #-}

-- | Holds a reference to the current monadic state, then calls the
-- provided fn with a restore action, which can be used to restore
-- the original state.
backingUpState :: forall s b sig1 m1 sig2 m2.
                  Has (State s) sig1 m1
               => Has (State s) sig2 m2
               => (m2 () -> m1 b) -> m1 b
backingUpState action = do
  s <- get @s
  action (put @s s)

swapEither :: Either b a -> Either a b
swapEither (Left a) = Right a
swapEither (Right b) = Left b
{-# INLINE swapEither #-}

rightToMaybe :: Either b a -> Maybe a
rightToMaybe (Right r) = Just r
rightToMaybe _         = Nothing
{-# INLINE rightToMaybe #-}

whileM :: Monad m => m Bool -> m () -> m ()
whileM cond action = do
  b <- cond
  if not b
  then pure ()
  else do
    action
    whileM cond action

foldMapIntersperse :: (Foldable t, Monoid m) => (a -> m) -> m -> t a -> m
foldMapIntersperse toMonoid separator elems =
  case toList elems of
    [] -> mempty
    (a:as) -> toMonoid a <> (foldMap ((separator <>) . toMonoid) as)

modifyM :: Has (State s) sig m => (s -> m s) -> m ()
modifyM f = do
  s <- get
  s <- f s
  put s
