{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.TreeWalk.Interpreter where

import Control.Carrier.Empty.Church
import Control.Effect.Trace
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Text as T
import HSLox.ASTPrinter (printAST)
import HSLox.ErrorReport (ErrorReport, toErrorReport)
import qualified HSLox.Scanner.Megaparsec as Scanner
import HSLox.Scanner.ScanError (ScanError)
import qualified HSLox.Parser.ByTheBook.Parser as Parser
import HSLox.Parser.ParserError (ParserError)
import qualified HSLox.Util as Util

interpret :: Has Trace sig m
          => T.Text -> m (Seq ErrorReport)
interpret source = do
  (scanErrors, tokens) <- Util.runWriterToPair @(Seq ScanError) $ Scanner.scanTokens source
  for_ tokens $ \token ->
    trace $ show token
  (parserErrors, exprs) <- Util.runWriterToPair @(Seq ParserError) $ Parser.parse tokens
  for_ exprs $ \expr ->
    trace . T.unpack $ "expr> " <> printAST expr
  pure $ (toErrorReport <$> scanErrors)
      <> (toErrorReport <$> parserErrors)
