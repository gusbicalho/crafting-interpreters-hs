module HSLox.ASTPrinterSpec where

import Data.Text qualified as T
import HSLox.AST qualified as AST
import HSLox.ASTPrinter (ASTPrinter (..))
import HSLox.Token (Token (..))
import HSLox.Token qualified as Token
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

bookExpr :: AST.ExprI
bookExpr =
  AST.BinaryE
    ( AST.UnaryE
        (Token "-" Token.MINUS Nothing 1)
        (AST.NumE 123)
    )
    (Token "*" Token.STAR Nothing 1)
    ( AST.GroupingE
        (AST.NumE 45.67)
    )

printedBiggerExpr :: T.Text
printedBiggerExpr = "(+ (group (- 3.0 (- 1.0))) \"asd\")"

biggerExpr :: AST.ExprI
biggerExpr =
  AST.BinaryE
    ( AST.GroupingE
        ( AST.BinaryE
            (AST.NumE 3)
            (Token "-" Token.MINUS Nothing 0)
            ( AST.UnaryE
                (Token "-" Token.MINUS Nothing 0)
                (AST.NumE 1)
            )
        )
    )
    (Token "+" Token.PLUS Nothing 0)
    (AST.StringE "asd")
