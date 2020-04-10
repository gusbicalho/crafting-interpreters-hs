module HSLox.TreeWalk.InterpreterSpec where

import Control.Algebra (run)
import Data.Function
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Tuple (swap)
import qualified HSLox.AST as AST
import HSLox.Output.Carrier.ToWriter
import qualified HSLox.Parser.Megaparsec as Parser
import HSLox.Parser.ParserError (ParserError)
import qualified HSLox.Scanner.Megaparsec as Scanner
import HSLox.Scanner.ScanError (ScanError)
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token
import qualified HSLox.TreeWalk.Interpreter as Interpreter
import qualified HSLox.TreeWalk.RTEnv as RTEnv
import HSLox.TreeWalk.RTError (RTError (..))
import qualified HSLox.Util as Util
import Test.Hspec

spec :: Spec
spec = do
  describe "TreeWalk evaluates correctly" $ do
    it "an empty program" $
      "" `shouldEvaluateTo`
        (Nothing, Seq.empty)
    it "a program with variables and local scope" $ do
      "var a = 1; { var a = a + 2; print a; }"
        `shouldEvaluateTo`
        (Nothing, Seq.fromList [ "3" ])
    it "a program with variables, local scope, and a runtime error" $
       ( "var a = \"global a\";\n"
      <> "var b = \"global b\";\n"
      <> "var c = \"global c\";\n"
      <> "{\n"
      <> "  var a = \"outer a\";\n"
      <> "  var b = \"outer b\";\n"
      <> "  {\n"
      <> "    var a = \"inner a\";\n"
      <> "    print a;\n"
      <> "    print b;\n"
      <> "    print c;\n"
      <> "  }\n"
      <> "  print a;\n"
      <> "  print b;\n"
      <> "  print c;\n"
      <> "}\n"
      <> "print a;\n"
      <> "print b;\n"
      <> "print c;\n"
      <> "print d; // error\n"
      <> "print \"never printed!\";\n"
      ) `shouldEvaluateTo`
        ( Just (RTError "Undefined variable 'd'." $ Token "d" Token.IDENTIFIER Nothing 20)
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
            ])

shouldEvaluateTo :: T.Text -> (Maybe RTError, Seq T.Text) -> Expectation
source `shouldEvaluateTo` (error, output) =
  (source & runParser
          & Interpreter.interpret RTEnv.newEnv
          & fmap snd
          & runOutputToWriter @T.Text Seq.singleton
          & Util.runWriterToPair @(Seq T.Text)
          & run
          & swap)
  `shouldBe` (error, output)

runParser :: T.Text -> AST.Program
runParser = run
          . fmap snd
          . Util.runWriterToPair @(Seq ParserError)
          . Parser.parse
          . run
          . fmap snd
          . Util.runWriterToPair @(Seq ScanError)
          . Scanner.scanTokens
