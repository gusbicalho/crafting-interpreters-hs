module HSLox.ASTPrinterSpec where

import qualified Data.Text as T
import HSLox.AST
import HSLox.ASTPrinter
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token
import Test.Hspec

spec :: Spec
spec = do
  describe "printAST" $ do
    it "should print the example from the book correctly" $ do
      printAST bookExpr `shouldBe` printedBookExpr
    it "should print a more complex expression correctly" $ do
      printAST biggerExpr `shouldBe` printedBiggerExpr

printedBookExpr :: T.Text
printedBookExpr = "(* (- 123.0) (group 45.67))"

bookExpr :: Expr
bookExpr =
  BinaryE
    (UnaryE
      (Token "-" Token.MINUS Nothing 1)
      (NumE 123))
    (Token "*" Token.STAR Nothing 1)
    (GroupingE
      (NumE 45.67))

printedBiggerExpr :: T.Text
printedBiggerExpr = "(+ (group (- 3.0 (- 1.0))) \"asd\")"

biggerExpr :: Expr
biggerExpr =
  BinaryE
    (GroupingE
      (BinaryE
        (NumE 3)
        (Token "-" Token.MINUS Nothing 0)
        (UnaryE
          (Token "-" Token.MINUS Nothing 0)
          (NumE 1))))
    (Token "+" Token.PLUS Nothing 0)
    (StringE "asd")
