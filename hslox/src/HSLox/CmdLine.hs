{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.CmdLine
  ( start, runDefaultRepl
  ) where

import Control.Carrier.Error.Church
import Control.Carrier.Empty.Maybe
import Control.Carrier.Lift
import Control.Carrier.Trace.Printing
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified HSLox.AST as AST
import HSLox.CmdLine.ReadLine
import HSLox.ErrorReport (ErrorReport, toErrorReport)
import qualified HSLox.Parser.Megaparsec as Parser
import HSLox.Parser.ParserError (ParserError)
import qualified HSLox.TreeWalk.Interpreter as Interpreter
import qualified HSLox.Scanner.Megaparsec as Scanner
import HSLox.Scanner.ScanError (ScanError)
import qualified HSLox.Util as Util
import Data.Foldable
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified System.Environment as Env
import System.Exit (exitWith, ExitCode (..))
import System.IO (stdin, stderr)

data Args = Args (Maybe FilePath)
  deriving (Eq, Show)

usage :: T.Text
usage = "hslox [FILEPATH or -]"

parseArgs :: [String] -> Either T.Text Args
parseArgs [] = Right $ Args Nothing
parseArgs [path] = Right $ Args (Just path)
parseArgs _ = Left usage

start :: IO ()
start = do
  argstrs <- Env.getArgs
  case parseArgs argstrs of
    Left errMsg -> do
      T.IO.hPutStrLn stderr errMsg
      exitWith (ExitFailure 64)
    Right (Args maybePath) -> do
      runApp $
        case maybePath of
          Just path -> runFromSourceFile path
          Nothing -> runRepl

runDefaultRepl :: IO ()
runDefaultRepl = runApp runRepl

runApp :: _ a -> IO a
runApp app =
  app & runReadLine
      & runTrace
      & runM @IO

runFromSourceFile :: _ => FilePath -> m ()
runFromSourceFile path =
    runSource =<< getSource path
  where
    getSource "-"  = sendM @IO $ T.IO.hGetContents stdin
    getSource path = sendM @IO $ T.IO.readFile path

readSource :: Algebra sig m
           => T.Text
           -> m (Either (Seq ScanError, Seq ParserError) (Seq AST.Expr))
readSource source = do
  (scanErrors, tokens) <- Util.runWriterToPair @(Seq ScanError) $ Scanner.scanTokens source
  (parserErrors, exprs) <- Util.runWriterToPair @(Seq ParserError) $ Parser.parse tokens
  if (scanErrors /= Seq.empty || parserErrors /= Seq.empty)
  then pure $ Left (scanErrors, parserErrors)
  else pure $ Right exprs

runSource :: _ => T.Text -> m ()
runSource source = do
  exprs' <- readSource source
  case exprs' of
    Left readErrors -> do
      reportReadErrors readErrors
      sendM @IO $ exitWith (ExitFailure 65)
    Right exprs -> do
      (_, rtError) <- Interpreter.interpretExprs exprs
      for_ rtError $ \error -> do
        sendM @IO $ putStrLn (show error)
        sendM @IO $ exitWith (ExitFailure (70))

runRepl :: _ => m ()
runRepl = Util.untilEmpty $ do
  line <- readLine
  guard (line /= ":e")
  exprs' <- readSource line
  case exprs' of
    Left readErrors -> do
      reportReadErrors readErrors
    Right exprs -> do
      (values, rtError) <- Interpreter.interpretExprs exprs
      for_ values $ \value -> do
        sendM @IO . putStrLn . T.unpack $ (Interpreter.showValue value)
      for_ rtError $ \error -> do
        sendM @IO $ putStrLn (show error)

reportReadErrors :: Has Trace sig m => (Seq ScanError, Seq ParserError) -> m ()
reportReadErrors (scanErrors, parserErrors) =
  reportErrors $ (toErrorReport <$> scanErrors)
              <> (toErrorReport <$> parserErrors)

reportErrors :: _ => f ErrorReport -> m ()
reportErrors errors = for_ errors (trace . show)
