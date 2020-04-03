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
import HSLox.Parser.ParserError (ParserError)
import HSLox.Token (Token (..))
import qualified HSLox.Util as Util
import Test.Hspec

spec :: Spec
spec = do
  describe "expression without identifiers and keywords" $ do
    let tokens = run
               . fmap snd
               . Util.runWriterToPair @(Seq ScanError)
               . Scanner.scanTokens
               $ "1 / 5 + 2 * 4 / 3 - 6 / 3 / 2 + ---1 * !!!true?false,5:7,8 == 9"
    let expected = ( Seq.empty
                   , Seq.singleton "(, (?: (+ (- (+ (/ 1.0 5.0) (/ (* 2.0 4.0) 3.0)) (/ (/ 6.0 3.0) 2.0)) (* (- (- (- 1.0))) (! (! (! True))))) (, False 5.0) 7.0) (== 8.0 9.0))")
    describe "ByTheBook" $
      it "parses correctly" $
        runParser ByTheBook.parse tokens `shouldReturn` expected

runParser :: _ -> Seq Token -> IO (Seq ParserError, Seq T.Text)
runParser parse = (fmap . fmap . fmap) printAST
                . runM @IO
                . Util.runWriterToPair @(Seq ParserError)
                . parse
