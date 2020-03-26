{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.Interpreter where

import Control.Effect.Trace
import qualified Data.Text as T

interpret :: _ => T.Text -> m ()
interpret source = do
  trace $ T.unpack source
