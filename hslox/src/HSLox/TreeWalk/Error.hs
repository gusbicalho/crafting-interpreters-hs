{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.TreeWalk.Error where

import Control.Effect.State
import qualified Data.Text as T

data Error
  = Error
  { errorLine :: Int
  , errorWhere :: T.Text
  , errorMessage :: T.Text
  }
  deriving (Eq, Show)

hadError :: _ => m Bool
hadError = gets @[Error] (not . null)

reportError :: _ => Error -> m ()
reportError error = modify (error :)
