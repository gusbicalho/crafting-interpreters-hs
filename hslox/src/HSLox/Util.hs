module HSLox.Util where

import Control.Carrier.Empty.Church
import Control.Monad (forever)

runEmptyToMaybe :: Applicative m => EmptyC m a -> m (Maybe a)
runEmptyToMaybe = runEmpty (pure Nothing) (pure . Just)

runEmptyToUnit :: Applicative m => EmptyC m a -> m ()
runEmptyToUnit = runEmpty (pure ()) (const $ pure ())

untilEmpty :: Applicative m => EmptyC m a -> m ()
untilEmpty = runEmptyToUnit . forever

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust ma f = maybe (pure ()) f ma
