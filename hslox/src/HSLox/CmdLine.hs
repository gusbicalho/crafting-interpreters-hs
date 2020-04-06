{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.CmdLine
  ( start, runDefaultRepl
  ) where

import Control.Carrier.Empty.Maybe
import Control.Carrier.Lift
import Control.Carrier.Trace.Printing
import HSLox.CmdLine.ReadLine
import HSLox.ErrorReport (ErrorReport)
import qualified HSLox.TreeWalk.Interpreter as Interpreter
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

runSource :: _ => T.Text -> m ()
runSource source = do
  result <- Interpreter.interpret source
  case result of
    Left compileErrorReports -> do
      reportErrors compileErrorReports
      sendM @IO $ exitWith (ExitFailure 65)
    Right (values, rtError) -> do
      for_ values $ \value -> do
        sendM @IO $ putStrLn (show value)
      for_ rtError $ \error -> do
        sendM @IO $ putStrLn (show error)
        sendM @IO $ exitWith (ExitFailure (-1))

runRepl :: _ => m ()
runRepl = Util.untilEmpty $ do
  line <- readLine
  guard (line /= ":e")
  result <- Interpreter.interpret line
  case result of
    Left compileErrorReports -> do
      reportErrors compileErrorReports
    Right (values, rtError) -> do
      for_ values $ \value -> do
        sendM @IO $ putStrLn (show value)
      for_ rtError $ \error -> do
        sendM @IO $ putStrLn (show error)

reportErrors :: _ => f ErrorReport -> m ()
reportErrors errors = for_ errors (trace . show)
