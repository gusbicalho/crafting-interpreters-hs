module HSLox.TreeWalk.RTReturn where

import Control.Carrier.Error.Church
import HSLox.TreeWalk.RTError (RTError)
import qualified HSLox.TreeWalk.RTError as RTError
import HSLox.TreeWalk.RTValue (RTValue)
import HSLox.Token (Token)

data RTReturn = RTReturn Token RTValue

runReturn :: Has (Throw RTError) sig m => ErrorC RTReturn m b -> m b
runReturn = runError @RTReturn returnOutsideFunctionCall pure
  where
    returnOutsideFunctionCall (RTReturn tk _) =
      RTError.throwRT tk $ "Unexpected 'return' outside function body"

throwReturn :: Has (Throw RTReturn) sig m => Token -> RTValue -> m a
throwReturn token value = throwError (RTReturn token value)

catchReturn :: Has (Catch RTReturn) sig m => m RTValue -> m RTValue
catchReturn action = action `catchError` \(RTReturn _ val) -> pure val
