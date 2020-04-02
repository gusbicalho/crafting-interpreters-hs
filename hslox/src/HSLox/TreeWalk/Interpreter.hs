{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.TreeWalk.Interpreter where

import Control.Carrier.Empty.Church
import Control.Carrier.State.Church
import Control.Effect.Trace
import Data.Foldable
import qualified Data.Text as T
import HSLox.Error (Error)
import HSLox.ASTPrinter (printAST)
import qualified HSLox.Scanner.Megaparsec as Scanner
import qualified HSLox.Parser.ByTheBook.Parser as Parser

interpret :: Has Trace sig m
          => T.Text -> m [Error]
interpret source
  = fmap reverse
  . execState @[Error] []
  $ do
    tokens <- Scanner.scanTokens source
    for_ tokens $ \token ->
      trace $ show token
    exprs <- Parser.parse tokens
    for_ exprs $ \expr ->
      trace . T.unpack $ "expr> " <> printAST expr
    pure ()
