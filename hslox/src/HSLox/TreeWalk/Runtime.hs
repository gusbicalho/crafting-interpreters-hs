module HSLox.TreeWalk.Runtime
  ( Trace, trace
  , module HSLox.TreeWalk.Runtime
  ) where

import Control.Effect.Trace
import Control.Effect.Error
import Control.Effect.State
import HSLox.NativeFns.Effect
import HSLox.TreeWalk.RTError (RTError (..))
import HSLox.TreeWalk.RTReturn (RTReturn (..))
import HSLox.TreeWalk.RTState (RTState (..))

type Runtime sig m = ( Has NativeFns sig m
                     , Has (Error RTError) sig m
                     , Has (Error RTReturn) sig m
                     , Has (State RTState) sig m
                     )
