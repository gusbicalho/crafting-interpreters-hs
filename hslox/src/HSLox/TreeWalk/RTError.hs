module HSLox.TreeWalk.RTError (
  RTError (..),
  module HSLox.TreeWalk.RTError,
) where

import Control.Algebra (Has)
import Control.Effect.Error (Throw)
import Control.Effect.Error qualified as Error
import Data.Text qualified as T
import HSLox.Token (Token)
import HSLox.TreeWalk.Runtime (RTError (..))

throwRT :: Has (Throw RTError) sig m => Token -> T.Text -> m a
throwRT tk msg = Error.throwError $ RTError msg tk
