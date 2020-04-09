module HSLox.TreeWalk.RTError where

import Control.Effect.Error
import qualified Data.Text as T
import HSLox.Token (Token)

data RTError = RTError { rtErrorMessage :: T.Text
                       , rtErrorToken :: Token
                       }
  deriving (Eq, Show)

throwRT :: Has (Throw RTError) sig m => Token -> T.Text -> m a
throwRT tk msg = throwError $ RTError msg tk
