{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.ScannerSpec where

import Control.Carrier.Lift
import Control.Carrier.State.Church
import Control.Carrier.Trace.Printing
import Data.Sequence (Seq)
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import HSLox.TreeWalk.Error
import qualified HSLox.Scanner.ByTheBook.Scanner as ByTheBook
import qualified HSLox.Scanner.Megaparsec as Megaparsec
import HSLox.TreeWalk.Token
import qualified HSLox.TreeWalk.TokenType as TokenType
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
testSource = "//first comment\n{123.456.789\nand.123.treco&// zuera\n \"lol\" )!=!<=<>=>/bla \"erro"

expectedResults :: ([Error], Seq Token)
expectedResults = ( [ Error {errorLine = 4, errorWhere = "", errorMessage = "Unterminated string."}
                    , Error {errorLine = 3, errorWhere = "", errorMessage = "Unexpected character: &"}
                    ]
                  , Seq.fromList
                     [ Token "{"       TokenType.LEFT_BRACE    Nothing                  2
                     , Token "123.456" TokenType.NUMBER        (Just (LitNum 123.456))  2
                     , Token "."       TokenType.DOT           Nothing                  2
                     , Token "789"     TokenType.NUMBER        (Just (LitNum 789.0))    2
                     , Token "and"     TokenType.AND           Nothing                  3
                     , Token "."       TokenType.DOT           Nothing                  3
                     , Token "123"     TokenType.NUMBER        (Just (LitNum 123.0))    3
                     , Token "."       TokenType.DOT           Nothing                  3
                     , Token "treco"   TokenType.IDENTIFIER    Nothing                  3
                     , Token "\"lol\"" TokenType.STRING        (Just (LitString "lol")) 4
                     , Token ")"       TokenType.RIGHT_PAREN   Nothing                  4
                     , Token "!="      TokenType.BANG_EQUAL    Nothing                  4
                     , Token "!"       TokenType.BANG          Nothing                  4
                     , Token "<="      TokenType.LESS_EQUAL    Nothing                  4
                     , Token "<"       TokenType.LESS          Nothing                  4
                     , Token ">="      TokenType.GREATER_EQUAL Nothing                  4
                     , Token ">"       TokenType.GREATER       Nothing                  4
                     , Token "/"       TokenType.SLASH         Nothing                  4
                     , Token "bla"     TokenType.IDENTIFIER    Nothing                  4
                     , Token ""        TokenType.EOF           Nothing                  4
                     ]
                  )

runScan :: (a -> _ (Seq Token)) -> a -> IO ([Error], (Seq Token))
runScan scanTokens = runM @IO
                   . runTrace
                   . runState @[Error] (\s a -> pure (s,a)) []
                   . scanTokens
