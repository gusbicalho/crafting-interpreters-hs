module HSLox.TreeWalk.Runtime
  ( Trace, trace
  , module HSLox.TreeWalk.Runtime
  ) where

import Control.Effect.Trace
import Control.Effect.Error
import Control.Effect.State
import HSLox.NativeFns.Effect
import HSLox.TreeWalk.RTState (RTState (..))
import HSLox.TreeWalk.RTError (RTError (..))

type Runtime sig m = ( Has NativeFns sig m
                     , Has (Error RTError) sig m
                     , Has (State RTState) sig m
                     )
