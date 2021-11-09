module HSLox.TreeWalk.RTReturn (
  RTReturn (..),
  module HSLox.TreeWalk.RTReturn,
) where

import Control.Algebra (Has)
import Control.Carrier.Error.Church qualified as Error.Church
import Control.Effect.Error (Catch, Throw)
import Control.Effect.Error qualified as Error
import HSLox.Token (Token)
import HSLox.TreeWalk.RTError qualified as RTError
import HSLox.TreeWalk.Runtime (RTError, RTReturn (..), RTValue)

runReturn ::
  forall cell sig m b.
  Has (Throw RTError) sig m =>
  Error.Church.ErrorC (RTReturn cell) m b ->
  m b
runReturn = Error.Church.runError @(RTReturn cell) returnOutsideFunctionCall pure
 where
  returnOutsideFunctionCall (RTReturn tk _) =
    RTError.throwRT tk "Unexpected 'return' outside function body"
{-# INLINE runReturn #-}

throwReturn :: Has (Throw (RTReturn cell)) sig m => Token -> RTValue cell -> m a
throwReturn token value = Error.throwError (RTReturn token value)

catchReturn :: Has (Catch (RTReturn cell)) sig m => m (RTValue cell) -> m (RTValue cell)
catchReturn action = action `Error.catchError` \(RTReturn _ val) -> pure val
