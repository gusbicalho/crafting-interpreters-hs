{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.ParserSpec where

import Control.Carrier.Lift
import Control.Carrier.State.Church
import Control.Carrier.Trace.Printing
import Data.Sequence (Seq)
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import HSLox.AST
import HSLox.ASTPrinter (printAST)
import HSLox.Error
import qualified HSLox.Scanner.Megaparsec as Scanner
import qualified HSLox.Parser.ByTheBook.Parser as ByTheBook
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token
import Test.Hspec

spec :: Spec
spec = do
  describe "expression without identifiers and keywords" $ do
    let tokens = run
               . evalState @[Error] []
               . Scanner.scanTokens
               $ "1 / 5 + 2 * 4 / 3 - 6 / 3 / 2 + ---1 * !!!true"
    let expected = ( []
                   , Seq.singleton "(+ (- (+ (/ 1.0 5.0) (/ (* 2.0 4.0) 3.0)) (/ (/ 6.0 3.0) 2.0)) (* (- (- (- 1.0))) (! (! (! True)))))")
    describe "ByTheBook" $
      it "parses correctly" $
        runParser ByTheBook.parse tokens `shouldReturn` expected

runParser :: _ -> Seq Token -> IO ([Error], Seq T.Text)
runParser parse = (fmap . fmap . fmap) printAST
                . runM @IO
                . runState @[Error] (\s a -> pure (s,a)) []
                . parse
