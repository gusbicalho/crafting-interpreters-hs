{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.TreeWalk.Interpreter where

import Control.Carrier.Empty.Church
import Control.Carrier.State.Church
import Control.Effect.Trace
import Data.Foldable
import qualified Data.Text as T
import qualified HSLox.TreeWalk.Scanner as Scanner

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

interpret :: Has Trace sig m
          => T.Text -> m [Error]
interpret source
  = fmap reverse
  . execState @[Error] []
  $ do
    for_ (Scanner.scanTokens source) $ \token ->
      trace $ show token
    pure ()
