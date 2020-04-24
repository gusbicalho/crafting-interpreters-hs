{-# LANGUAGE StrictData #-}
module HSLox.TreeWalk.RTReturn
  ( RTReturn (..)
  , module HSLox.TreeWalk.RTReturn
  ) where

import Control.Carrier.Error.Church
import HSLox.TreeWalk.RTError (RTError)
import qualified HSLox.TreeWalk.RTError as RTError
import HSLox.TreeWalk.Runtime (RTReturn (..), RTValue)
import HSLox.Token (Token)

runReturn :: forall cell sig m b. Has (Throw RTError) sig m => ErrorC (RTReturn cell) m b -> m b
runReturn = runError @(RTReturn cell) returnOutsideFunctionCall pure
  where
    returnOutsideFunctionCall (RTReturn tk _) =
      RTError.throwRT tk $ "Unexpected 'return' outside function body"
{-# INLINE runReturn #-}

throwReturn :: Has (Throw (RTReturn cell)) sig m => Token -> (RTValue cell) -> m a
throwReturn token value = throwError (RTReturn token value)

catchReturn :: Has (Catch (RTReturn cell)) sig m => m (RTValue cell) -> m (RTValue cell)
catchReturn action = action `catchError` \(RTReturn _ val) -> pure val
