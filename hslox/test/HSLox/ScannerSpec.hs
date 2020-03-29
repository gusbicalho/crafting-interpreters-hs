{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.ScannerSpec where

import Control.Carrier.State.Church
import Data.Sequence (Seq)
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import HSLox.TreeWalk.Error
import qualified HSLox.Scanner.ByTheBook.Scanner as ByTheBook
import HSLox.TreeWalk.Token
import qualified HSLox.TreeWalk.TokenType as TokenType
import Test.Hspec

spec :: Spec
spec = do
  describe "ByTheBook" $
   it "scans correctly" $
     runScan ByTheBook.scanTokens testSource `shouldBe` expectedResults

testSource :: T.Text
testSource = "{123.456.789\nand.123.treco// zuera\n \"lol\" ) \"erro"

expectedResults :: ([Error], Seq Token)
expectedResults = ( [Error {errorLine = 3, errorWhere = "", errorMessage = "Unterminated string."}]
                  , Seq.fromList
                     [ Token "{"       TokenType.LEFT_BRACE  Nothing                  1
                     , Token "123.456" TokenType.NUMBER      (Just (LitNum 123.456))  1
                     , Token "."       TokenType.DOT         Nothing                  1
                     , Token "789"     TokenType.NUMBER      (Just (LitNum 789.0))    1
                     , Token "and"     TokenType.AND         Nothing                  2
                     , Token "."       TokenType.DOT         Nothing                  2
                     , Token "123"     TokenType.NUMBER      (Just (LitNum 123.0))    2
                     , Token "."       TokenType.DOT         Nothing                  2
                     , Token "treco"   TokenType.IDENTIFIER  Nothing                  2
                     , Token "\"lol\"" TokenType.STRING      (Just (LitString "lol")) 3
                     , Token ")"       TokenType.RIGHT_PAREN Nothing                  3
                     , Token ""        TokenType.EOF         Nothing                  3
                     ]
                  )

runScan :: (a -> _ (Seq Token)) -> a -> ([Error], (Seq Token))
runScan scanTokens = run
                   . runState @[Error] (\s a -> pure (s,a)) []
                   . scanTokens
