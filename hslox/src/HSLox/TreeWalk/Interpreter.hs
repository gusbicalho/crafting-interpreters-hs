{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.TreeWalk.Interpreter where

import Control.Carrier.Empty.Church
import Control.Carrier.State.Church
import Control.Effect.Trace
import Data.Foldable
import qualified Data.Text as T
import HSLox.TreeWalk.Error (Error)
import qualified HSLox.TreeWalk.Scanner as Scanner

interpret :: Has Trace sig m
          => T.Text -> m [Error]
interpret source
  = fmap reverse
  . execState @[Error] []
  $ do
    tokens <- Scanner.scanTokens source
    for_ tokens $ \token ->
      trace $ show token
    pure ()
