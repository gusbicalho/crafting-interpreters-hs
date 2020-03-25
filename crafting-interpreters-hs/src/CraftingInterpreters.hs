{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module CraftingInterpreters
  ( start
  ) where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.Trace.Printing
import Data.Function

start :: IO ()
start = app
      & runTrace
      & runReader (42 :: Int)
      & runM @IO

app :: _ => m ()
app = do
  trace "Running app"
  answer <- ask @Int
  trace $ "The answer is " <> show answer
