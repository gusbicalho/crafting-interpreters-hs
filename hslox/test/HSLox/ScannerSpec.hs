{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module HSLox.ScannerSpec where

import Control.Carrier.Lift qualified as Lift
import Control.Carrier.Trace.Printing qualified as Trace.Printing
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import HSLox.Scanner.ByTheBook.Scanner qualified as ByTheBook
import HSLox.Scanner.Megaparsec qualified as Megaparsec
import HSLox.Scanner.ScanError (ScanError (..))
import HSLox.Token (Token (..))
import HSLox.Token qualified as Token
import HSLox.Util qualified as Util
import Test.Hspec

spec :: Spec
spec = do
  describe "ByTheBook" $
    it "scans correctly" $
      runScan ByTheBook.scanTokens testSource `shouldReturn` expectedResults
  describe "Megaparsec" $
    it "scans correctly" $
      runScan Megaparsec.scanTokens testSource `shouldReturn` expectedResults

testSource :: T.Text
testSource = "//first comment\n{123.456.789\nand.123.treco&?:// zuera\n \"lol\" )!=!<=<>=>/bla \"erro"

expectedResults :: (Set ScanError, Seq Token)
expectedResults =
  ( Set.fromList
      [ ScanError 3 "" "Unexpected character: &"
      , ScanError 4 "" "Unterminated string."
      ]
  , Seq.fromList
      [ Token "{" Token.LEFT_BRACE Nothing 2
      , Token "123.456" Token.NUMBER (Just $ Token.LitNum 123.456) 2
      , Token "." Token.DOT Nothing 2
      , Token "789" Token.NUMBER (Just $ Token.LitNum 789.0) 2
      , Token "and" Token.AND Nothing 3
      , Token "." Token.DOT Nothing 3
      , Token "123" Token.NUMBER (Just $ Token.LitNum 123.0) 3
      , Token "." Token.DOT Nothing 3
      , Token "treco" Token.IDENTIFIER Nothing 3
      , Token "?" Token.QUESTION_MARK Nothing 3
      , Token ":" Token.COLON Nothing 3
      , Token "\"lol\"" Token.STRING (Just $ Token.LitString "lol") 4
      , Token ")" Token.RIGHT_PAREN Nothing 4
      , Token "!=" Token.BANG_EQUAL Nothing 4
      , Token "!" Token.BANG Nothing 4
      , Token "<=" Token.LESS_EQUAL Nothing 4
      , Token "<" Token.LESS Nothing 4
      , Token ">=" Token.GREATER_EQUAL Nothing 4
      , Token ">" Token.GREATER Nothing 4
      , Token "/" Token.SLASH Nothing 4
      , Token "bla" Token.IDENTIFIER Nothing 4
      , Token "" Token.EOF Nothing 4
      ]
  )

runScan :: (a -> _ (Seq Token)) -> a -> IO (Set ScanError, Seq Token)
runScan scanTokens =
  Lift.runM @IO
    . Trace.Printing.runTrace
    . Util.runWriterToPair @(Set ScanError)
    . scanTokens
