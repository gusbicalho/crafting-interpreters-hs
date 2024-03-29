{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module HSLox.TreeWalk.InterpreterSpec where

import Control.Carrier.Lift qualified as Lift
import Data.Function ((&))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Text qualified as T
import Data.Tuple (swap)
import HSLox.AST qualified as AST
import HSLox.Cells.Carrier.CellsOnST qualified as CellsOnST
import HSLox.Parser.Megaparsec qualified as Parser
import HSLox.Parser.ParserError (ParserError)
import HSLox.Scanner.Megaparsec qualified as Scanner
import HSLox.Scanner.ScanError (ScanError)
import HSLox.StaticAnalysis.Analyzer qualified as Analyzer
import HSLox.Test.NativeFnsMock qualified as NativeFnsMock
import HSLox.Token (Token (..))
import HSLox.Token qualified as Token
import HSLox.TreeWalk.Interpreter (InterpreterError (..))
import HSLox.TreeWalk.Interpreter qualified as Interpreter
import HSLox.Util qualified as Util
import Test.Hspec

spec :: Spec
spec = do
  describe "TreeWalk evaluates correctly" $ do
    it "an empty program" $
      ""
        `shouldEvaluateTo` (Nothing, Seq.empty)
    it "a program with variables and local scope" $ do
      "var a = 1; { var temp = a + 2; var a = temp; print(a); }"
        `shouldEvaluateTo` (Nothing, Seq.fromList ["3"])
    it "a program with variables, local scope, and a runtime error" $
      ( "var a = \"global a\";\n"
          <> "var b = \"global b\";\n"
          <> "var c = \"global c\";\n"
          <> "{\n"
          <> "  var a = \"outer a\";\n"
          <> "  var b = \"outer b\";\n"
          <> "  {\n"
          <> "    var a = \"inner a\";\n"
          <> "    print(a);\n"
          <> "    print(b);\n"
          <> "    print(c);\n"
          <> "  }\n"
          <> "  print(a);\n"
          <> "  print(b);\n"
          <> "  print(c);\n"
          <> "}\n"
          <> "print(a);\n"
          <> "print(b);\n"
          <> "print(c);\n"
          <> "print(d); // error\n"
          <> "print(\"never printed!\");\n"
      )
        `shouldEvaluateTo` ( Just
                            ( InterpreterError
                                Interpreter.StageRun
                                "Undefined variable 'd'."
                                $ Token "d" Token.IDENTIFIER Nothing 20
                            )
                           , Seq.fromList
                            [ "inner a"
                            , "outer b"
                            , "global c"
                            , "outer a"
                            , "outer b"
                            , "global c"
                            , "global a"
                            , "global b"
                            , "global c"
                            ]
                           )
    describe "a program with conditionals and local scope" $ do
      let source condition = "var x = 1; if (" <> condition <> ") print(x); else { x = 3; var x = 2; print(x); } print(x);"
      it "taking the 'then' branch" $ do
        source "3 == 3"
          `shouldEvaluateTo` (Nothing, Seq.fromList ["1", "1"])
      it "taking the 'else' branch" $ do
        source "3 != 3"
          `shouldEvaluateTo` (Nothing, Seq.fromList ["2", "3"])
    it "a program with logical operators" $ do
      "if (false or true and false) print(1); else print(2);"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["2"]
                           )
    it "a program with while statement" $ do
      "var x = 0; while (x < 5) { print(x); x = x + 1; }"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["0", "1", "2", "3", "4"]
                           )
    it "a program that prints the first Fibonacci numbers" $ do
      "var a = 0; var b = 1; while (a < 100) { print(b); var temp = a; a = b; b = b + temp; }"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["1", "1", "2", "3", "5", "8", "13", "21", "34", "55", "89", "144"]
                           )
    it "a program with for statement" $ do
      "for (var x = 0; x < 5; x = x + 1) { print(x); }"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["0", "1", "2", "3", "4"]
                           )
    it "a program that tries to call a number as a function and fails" $
      do
        "print((1 + 3)(5));"
        `shouldEvaluateTo` ( Just
                            ( InterpreterError
                                Interpreter.StageRun
                                "Can only call functions and classes."
                                $ Token ")" Token.RIGHT_PAREN Nothing 1
                            )
                           , Seq.empty
                           )
    it "a program the calls clock()" $
      do
        "print(clock()); print(clock()); print(clock());"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["0", "1", "2"]
                           )
    it "a program the defines and calls a function" $
      do
        "fun square(x) { print(x*x); } square(4);"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["16"]
                           )
    it "a program that uses the return value of a function" $
      do
        "fun square(x) { return x*x; } print(square(4));"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["16"]
                           )
    it "a global recursive function" $
      do
        "fun factorial(x) { if (x <= 0) return 1; else return x*factorial(x-1); } print(factorial(4));"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["24"]
                           )
    it "a program with a closure" $
      do
        mconcat
          [ "fun makeInc() { var x = 0; fun inc() { x = x + 1; return x; } return inc; } "
          , "var inc = makeInc(); print(inc()); print(inc());"
          ]
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["1", "2"]
                           )
    it "a program with a closure that does not capture a variable declared after the fn" $
      do
        "var x = 1; { fun printX() { print(x); } printX(); var x = 42; printX(); }"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["1", "1"]
                           )
    it "mutually recursive fns" $
      do
        mconcat
          [ "{"
          , "  var even; "
          , "  fun odd(n) { if (n == 0) return false; return even(n-1); } "
          , "  fun even(n) { if (n == 0) return true; return odd(n-1); } "
          , "  print(odd(4)); print(even(4)); "
          , "}"
          ]
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["false", "true"]
                           )
    it "a program with anonymous fn expressions" $
      do
        "fun twice(f) { return fun (a) { f(a); f(a); }; } twice(fun (a) { print(a); })(3);"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["3", "3"]
                           )
    it "a program with classes and object field access" $
      do
        "class A { } var a = A(); a.play = 3; print(a.play);"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["3"]
                           )
    it "a program with classes and object method access" $
      do
        "class A { play() { print(3); }} var a = A(); a.play();"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["3"]
                           )
    it "a program with classes and methods that use `this`" $
      do
        "class A { play() { print(this.song); }} var a = A(); a.song = \"The Lemon Song\"; var play = a.play; play();"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["The Lemon Song"]
                           )
    it "a program with a class with a constructor" $
      do
        "class A { init(i) { this.i = i; } } print(A(7).i);"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["7"]
                           )
    it "a program with classes and subclasses" $
      do
        "class A { one() { return 1; } } class B < A { two() { return 2; } } var b = B(); print(b.one()+b.two());"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["3"]
                           )
    it "a program that uses `super`" $
      do
        "class Klass { one(a) { print(a); } } class Qlass < Klass { one() { super.one(4); print(1); } } Qlass().one();"
        `shouldEvaluateTo` ( Nothing
                           , Seq.fromList ["4", "1"]
                           )

shouldEvaluateTo :: T.Text -> (Maybe InterpreterError, Seq T.Text) -> Expectation
source `shouldEvaluateTo` (error, output) =
  CellsOnST.runST evaluate
    `shouldBe` (error, output)
 where
  evaluate :: forall s. CellsOnST.ST s (Maybe InterpreterError, Seq T.Text)
  evaluate =
    source & runParser
      & Interpreter.interpret @(CellsOnST.Cell s)
      & NativeFnsMock.runNativeFnsMock
      & CellsOnST.runCellsOnST @s
      & Util.runWriterToPair @(Seq T.Text)
      & Lift.runM @(CellsOnST.ST s)
      & fmap swap

runParser :: T.Text -> AST.Program _
runParser source =
  source
    & Scanner.scanTokens
    & Util.runWriterToPair @(Set ScanError)
    & fmap snd
    & Lift.run
    & Parser.parse
    & Util.runWriterToPair @(Set ParserError)
    & fmap snd
    & (Analyzer.analyze =<<)
    & Util.runWriterToPair @(Set Analyzer.AnalysisError)
    & fmap snd
    & Lift.run
