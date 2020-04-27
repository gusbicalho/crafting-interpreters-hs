{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.CmdLine
  ( start, runDefaultRepl, runText
  ) where

import Control.Carrier.Error.Church
import Control.Carrier.State.Church
import Control.Carrier.Lift
import Control.Carrier.Trace.Printing
import Control.Effect.Empty
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified HSLox.AST as AST
import qualified HSLox.Cells.Carrier.CellsOnIO as CellsOnIO
import HSLox.CmdLine.ReadLine
import HSLox.ErrorReport (ErrorReport, toErrorReport)
import qualified HSLox.NativeFns.Carrier.NativeFnsOnIO as NativeFnsOnIO
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
import System.IO (hPutStrLn, stdin, stderr)

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

runText :: T.Text -> IO ()
runText = runApp . runSource

runApp :: _ a -> IO a
runApp app =
  app & NativeFnsOnIO.runNativeFnsOnIO
      & CellsOnIO.runCellsOnIO
      & runReadLine
      & runTrace
      & runM @IO
{-# INLINE runApp #-}

runFromSourceFile :: _ => FilePath -> m ()
runFromSourceFile path =
    runSource =<< getSource path
  where
    getSource "-"  = sendM @IO $ T.IO.hGetContents stdin
    getSource path = sendM @IO $ T.IO.readFile path
{-# INLINE runFromSourceFile #-}

readSource :: Algebra sig m
           => T.Text
           -> m (Either (Set ScanError, Set ParserError) (AST.Program AST.Identity))
readSource source = do
  (scanErrors, tokens) <- Util.runWriterToPair @(Set ScanError) $ Scanner.scanTokens source
  (parserErrors, program) <- Util.runWriterToPair @(Set ParserError) $ Parser.parse tokens
  if (scanErrors /= Set.empty || parserErrors /= Set.empty)
  then pure $ Left (scanErrors, parserErrors)
  else pure $ Right program

runSource :: _ => T.Text -> m ()
runSource source = do
  exprs' <- readSource source
  case exprs' of
    Left readErrors -> do
      reportReadErrors readErrors
      sendM @IO $ exitWith (ExitFailure 65)
    Right exprs -> do
      rtError <- Interpreter.interpret @CellsOnIO.Cell exprs
      for_ rtError $ \error -> do
        sendM @IO $ hPutStrLn stderr (show error)
        sendM @IO $ exitWith (ExitFailure (70))
{-# INLINE runSource #-}

runRepl :: _ => m ()
runRepl = do
  newEnv <- Interpreter.baseEnv @CellsOnIO.Cell
  evalState newEnv . Util.untilEmpty $ do
    line <- readLine
    guard (line /= ":e")
    exprs' <- readSource line
    case exprs' of
      Left readErrors -> do
        reportReadErrors readErrors
      Right exprs -> do
        rtState <- get
        (rtState, rtError) <- Interpreter.interpretNext @CellsOnIO.Cell rtState exprs
        put rtState
        for_ rtError $ \error -> do
          sendM @IO $ putStrLn (show error)
{-# INLINE runRepl #-}

reportReadErrors :: Has Trace sig m => (Set ScanError, Set ParserError) -> m ()
reportReadErrors (scanErrors, parserErrors) = do
  reportErrors $ (toErrorReport <$> foldMap Seq.singleton scanErrors)
              <> (toErrorReport <$> foldMap Seq.singleton parserErrors)

reportErrors :: _ => f ErrorReport -> m ()
reportErrors errors = for_ errors (trace . show)
