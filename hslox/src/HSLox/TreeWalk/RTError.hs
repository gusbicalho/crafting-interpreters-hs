{-# LANGUAGE StrictData #-}
module HSLox.TreeWalk.RTError
  ( RTError (..)
  , module HSLox.TreeWalk.RTError
  ) where

import Control.Effect.Error
import qualified Data.Text as T
import HSLox.Token (Token)
import HSLox.TreeWalk.Runtime (RTError (..))

throwRT :: Has (Throw RTError) sig m => Token -> T.Text -> m a
throwRT tk msg = throwError $ RTError msg tk
