{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module HSLox.CmdLine (
  start,
  runDefaultRepl,
  runText,
) where

import Control.Algebra (Algebra, Has)
import Control.Carrier.Lift qualified as Lift
import Control.Carrier.State.Church qualified as State.Church
import Control.Carrier.Trace.Printing qualified as Trace.Printing
import Control.Effect.Empty qualified as Empty
import Control.Effect.State qualified as State
import Control.Effect.Trace (Trace)
import Control.Effect.Trace qualified as Trace
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import HSLox.AST qualified as AST
import HSLox.Cells.Carrier.CellsOnIO qualified as CellsOnIO
import HSLox.CmdLine.ReadLine (ReadLineC (runReadLine), readLine)
import HSLox.ErrorReport (ErrorReport, toErrorReport)
import HSLox.NativeFns.Carrier.NativeFnsOnIO qualified as NativeFnsOnIO
import HSLox.Parser.Megaparsec qualified as Parser
import HSLox.Parser.ParserError (ParserError)
import HSLox.Scanner.Megaparsec qualified as Scanner
import HSLox.Scanner.ScanError (ScanError)
import HSLox.StaticAnalysis.Analyzer (AnalysisError, analyze)
import HSLox.TreeWalk.Interpreter qualified as Interpreter
import HSLox.Util qualified as Util
import System.Environment qualified as Env
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr, stdin)

data Args = Args (Maybe FilePath)
  deriving stock (Eq, Show)

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
    & Trace.Printing.runTrace
    & Lift.runM @IO
{-# INLINE runApp #-}

runFromSourceFile :: _ => FilePath -> m ()
runFromSourceFile path =
  runSource =<< getSource path
 where
  getSource "-" = Lift.sendM @IO $ T.IO.hGetContents stdin
  getSource path = Lift.sendM @IO $ T.IO.readFile path
{-# INLINE runFromSourceFile #-}

readSource ::
  Algebra sig m =>
  T.Text ->
  m (Either (Set ScanError, Set ParserError, Set AnalysisError) (AST.Program _))
readSource source = do
  (scanErrors, tokens) <- Util.runWriterToPair @(Set ScanError) $ Scanner.scanTokens source
  (parserErrors, program) <- Util.runWriterToPair @(Set ParserError) $ Parser.parse tokens
  (resolverErrors, program) <- Util.runWriterToPair @(Set AnalysisError) $ analyze program
  if ( scanErrors /= Set.empty
        || parserErrors /= Set.empty
        || resolverErrors /= Set.empty
     )
    then pure $ Left (scanErrors, parserErrors, resolverErrors)
    else pure $ Right program

runSource :: _ => T.Text -> m ()
runSource source = do
  exprs' <- readSource source
  case exprs' of
    Left readErrors -> do
      reportReadErrors readErrors
      Lift.sendM @IO $ exitWith (ExitFailure 65)
    Right exprs -> do
      rtError <- Interpreter.interpret @CellsOnIO.Cell exprs
      for_ rtError $ \error -> do
        Lift.sendM @IO $ hPutStrLn stderr (show error)
        Lift.sendM @IO $ exitWith (ExitFailure (70))
{-# INLINE runSource #-}

runRepl :: _ => m ()
runRepl = do
  newEnv <- Interpreter.baseEnv @CellsOnIO.Cell
  State.Church.evalState newEnv . Util.untilEmpty $ do
    line <- readLine
    Empty.guard (line /= ":e")
    exprs' <- readSource line
    case exprs' of
      Left readErrors -> do
        reportReadErrors readErrors
      Right exprs -> do
        rtState <- State.get
        (rtState, rtError) <- Interpreter.interpretNext @CellsOnIO.Cell rtState exprs
        State.put rtState
        for_ rtError $ \error -> do
          Lift.sendM @IO $ print error
{-# INLINE runRepl #-}

reportReadErrors :: Has Trace sig m => (Set ScanError, Set ParserError, Set AnalysisError) -> m ()
reportReadErrors (scanErrors, parserErrors, resolverErrors) = do
  reportErrors $
    (toErrorReport <$> foldMap Seq.singleton scanErrors)
      <> (toErrorReport <$> foldMap Seq.singleton parserErrors)
      <> (toErrorReport <$> foldMap Seq.singleton resolverErrors)

reportErrors :: _ => f ErrorReport -> m ()
reportErrors errors = for_ errors (Trace.trace . show)
