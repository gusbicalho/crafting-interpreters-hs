{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.ParserSpec where

import Control.Carrier.Lift
import Data.Sequence (Seq)
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import HSLox.ASTPrinter (printAST)
import HSLox.Scanner.ScanError (ScanError)
import qualified HSLox.Scanner.Megaparsec as Scanner
import qualified HSLox.Parser.ByTheBook.Parser as ByTheBook
import qualified HSLox.Parser.Megaparsec as Megaparsec
import HSLox.Parser.ParserError (ParserError (..))
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token
import qualified HSLox.Util as Util
import Test.Hspec

spec :: Spec
spec = do
  describe "expression without identifiers and keywords" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "1 / 2 / 3 + 2 * 4 == 9 + 6 ? !!true : -false, 9 < 11")
        ( Seq.empty
        , Seq.singleton "(, (?: (== (+ (/ (/ 1.0 2.0) 3.0) (* 2.0 4.0)) (+ 9.0 6.0)) (! (! True)) (- False)) (< 9.0 11.0))"
        )
    describe "with error productions" $ do
      testParserImplementations
        (scan "1 / 2, < 11; \n+2+2; \n-1-1; \n*-4; == 7")
        ( Seq.fromList
            [ ParserError (Just $ Token "<"  Token.LESS        Nothing 1) "Binary operator < found at the beginning of expression."
            , ParserError (Just $ Token "+"  Token.PLUS        Nothing 2) "Binary operator + found at the beginning of expression."
            , ParserError (Just $ Token ";"  Token.SEMICOLON   Nothing 3) "Expect expression."
            , ParserError (Just $ Token "*"  Token.STAR        Nothing 4) "Binary operator * found at the beginning of expression."
            , ParserError (Just $ Token "==" Token.EQUAL_EQUAL Nothing 4) "Binary operator == found at the beginning of expression."
            ]
        , Seq.singleton "(- (- 1.0) 1.0)"
        )

testParserImplementations :: Seq Token
                          -> (Seq ParserError, Seq T.Text)
                          -> Spec
testParserImplementations tokens expected = do
  describe "ByTheBook" $
    it "parses correctly" $
      runParser ByTheBook.parse tokens `shouldReturn` expected
  describe "Megaparsec" $
    it "parses correctly" $
      runParser Megaparsec.parse tokens `shouldReturn` expected

runParser :: _ -> Seq Token -> IO (Seq ParserError, Seq T.Text)
runParser parse = (fmap . fmap . fmap) printAST
                . runM @IO
                . Util.runWriterToPair @(Seq ParserError)
                . parse

scan :: T.Text -> Seq Token
scan = run . fmap snd . Util.runWriterToPair @(Seq ScanError) . Scanner.scanTokens
