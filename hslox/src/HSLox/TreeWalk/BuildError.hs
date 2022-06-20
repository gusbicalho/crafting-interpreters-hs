module HSLox.TreeWalk.BuildError (
  BuildError (..),
  module HSLox.TreeWalk.BuildError,
) where

import Control.Algebra (Has)
import Control.Effect.Error (Throw)
import Control.Effect.Error qualified as Error
import Data.Text qualified as T
import HSLox.Token (Token)

data BuildError = BuildError
  { buildErrorMessage :: T.Text
  , buildErrorToken :: Token
  }
  deriving stock (Eq, Show)

throwBuildError :: Has (Throw BuildError) sig m => Token -> T.Text -> m a
throwBuildError tk msg = Error.throwError $ BuildError msg tk
