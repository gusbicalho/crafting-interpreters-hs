{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.CmdLine
  ( start, runDefaultRepl
  ) where

import Control.Carrier.Empty.Maybe
import Control.Carrier.Lift
import Control.Carrier.Trace.Printing
import Control.Monad (forever, void, when)
import HSLox.CmdLine.ReadLine
import HSLox.ErrorReport (ErrorReport)
import qualified HSLox.TreeWalk.Interpreter as Interpreter
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
  errors <- Interpreter.interpret source
  reportErrors errors
  when (not . null $ errors) $ do
    sendM @IO $ exitWith (ExitFailure 65)

runRepl :: _ => m ()
runRepl = void . runEmpty . forever $ do
  line <- readLine
  guard (line /= ":e")
  errors <- Interpreter.interpret line
  reportErrors errors

reportErrors :: _ => f ErrorReport -> m ()
reportErrors errors = for_ errors (trace . show)
