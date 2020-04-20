{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.ParserSpec where

import Control.Carrier.Lift
import Data.Functor
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
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
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC

spec :: Spec
spec = do
  describe "Implementations are equivalent" $ do
    prop "ByTheBook and Megaparsec implementations give the same results" $
      parserImplementationsAreEquivalent
  describe "empty program" $ do
    testParserImplementations
      (scan "")
      ( Seq.empty
      , "[ ]"
      )
  describe "expression without identifiers and keywords" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "1 / 2 / 3 + (2 * 4) == 9 + 6 ? !!true : -false, 9 < 11;")
        ( Seq.empty
        , "[ (, (?: (== (+ (/ (/ 1.0 2.0) 3.0) (group (* 2.0 4.0))) (+ 9.0 6.0)) (! (! True)) (- False)) (< 9.0 11.0)) ]"
        )
    describe "with error productions" $ do
      testParserImplementations
        (scan "1 / 2, < 11; \n+2+2; \n-1-1; \n*-4; == 7")
        ( Seq.fromList
            [ ParserError (Just $ Token "<"  Token.LESS        Nothing 1) "Binary operator < found at the beginning of expression."
            , ParserError (Just $ Token "+"  Token.PLUS        Nothing 2) "Binary operator + found at the beginning of expression."
            , ParserError (Just $ Token "*"  Token.STAR        Nothing 4) "Binary operator * found at the beginning of expression."
            , ParserError (Just $ Token "==" Token.EQUAL_EQUAL Nothing 4) "Binary operator == found at the beginning of expression."
            ]
        , "[ (- (- 1.0) 1.0) ]"
        )
  describe "programs with expression and print statements" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "120 / 2; print 123 + 4 * 7;")
        ( Seq.empty
        , "[ (/ 120.0 2.0) (print (+ 123.0 (* 4.0 7.0))) ]"
        )
  describe "programs with expression, print and declaration statements, identifier expressions and assignment" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "120 / 2; print 123 + 4 * 7; var x = 2 + 3; var y = 7; print x+y; x = y = 9; print x*y;")
        ( Seq.empty
        , "[ (/ 120.0 2.0) (print (+ 123.0 (* 4.0 7.0))) (var x (+ 2.0 3.0)) (var y 7.0) (print (+ x y)) (= x (= y 9.0)) (print (* x y)) ]"
        )
  describe "programs with blocks" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "var x = 120 / 2; print x; { var x = 7; print x; x = 3; print x; {} } print x;")
        ( Seq.empty
        , "[ (var x (/ 120.0 2.0)) (print x) { (var x 7.0) (print x) (= x 3.0) (print x) { } } (print x) ]"
        )
    describe "with unterminated block" $ do
      testParserImplementations
        (scan "var x = 120 / 2; print x; { var x = 7; print x; { var y = 7; }")
        ( Seq.fromList
            [ ParserError (Just $ Token "" Token.EOF Nothing 1) "Expect '}' after block."
            ]
        , "[ (var x (/ 120.0 2.0)) (print x) ]"
        )
    describe "with nested unterminated blocks" $ do
      testParserImplementations
        (scan "{ { } {")
        ( Seq.fromList
            [ ParserError (Just $ Token "" Token.EOF Nothing 1) "Expect '}' after block."
            ]
        , "[ ]"
        )
    describe "with unterminated statement inside block" $ do
      testParserImplementations
        (scan "var x = 120 / 2; print x; { var x = 7 }")
        ( Seq.fromList
            [ ParserError (Just $ Token "}" Token.RIGHT_BRACE Nothing 1) "Expect ';' after variable declaration."
            ]
        , "[ (var x (/ 120.0 2.0)) (print x) ]"
        )
  describe "programs with if statements" $ do
    describe "correct and nested" $ do
      testParserImplementations
        (scan "if (!(true == false)) if (false) print 1; else print 2; else { if (true) { print 5; } else if (false) print 7; }")
        ( Seq.empty
        , "[ (if (! (group (== True False))) (if False (print 1.0) (print 2.0)) { (if True { (print 5.0) } (if False (print 7.0))) }) ]"
        )
    describe "dangling else" $ do
      testParserImplementations
        (scan "if (first) if (second) print 1; else print 2;")
        ( Seq.empty
        , "[ (if first (if second (print 1.0) (print 2.0))) ]"
        )
    describe "incomplete ifs" $ do
      testParserImplementations
        (scan "if;\n if (;\n if (true;\n if (true) else;\n if (true) {} else;\n if (true) else {};\n {}")
        ( Seq.fromList
            [ ParserError (Just $ Token ";"    Token.SEMICOLON Nothing 1) "Expect '(' after 'if'."
            , ParserError (Just $ Token ";"    Token.SEMICOLON Nothing 2) "Expect expression."
            , ParserError (Just $ Token ";"    Token.SEMICOLON Nothing 3) "Expect ')' after if condition."
            , ParserError (Just $ Token "else" Token.ELSE      Nothing 4) "Expect expression."
            , ParserError (Just $ Token ";"    Token.SEMICOLON Nothing 5) "Expect expression."
            , ParserError (Just $ Token "else" Token.ELSE      Nothing 6) "Expect expression."
            ]
        , "[ { } ]")
  describe "programs with logical operators" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "if (false or true and false) print 1; else print 2;")
        ( Seq.empty
        , "[ (if (or False (and True False)) (print 1.0) (print 2.0)) ]")
  describe "programs with while statements" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "var x = 0; while (x < 5) { print x; x = x + 1; }")
        ( Seq.empty
        , "[ (var x 0.0) (while (< x 5.0) { (print x) (= x (+ x 1.0)) }) ]")
  describe "programs with for statements" $ do
    describe "loop forever" $ do
      testParserImplementations
        (scan "for (;;) print 1;")
        ( Seq.empty
        , "[ (while True (print 1.0)) ]")
    describe "count to 5" $ do
      testParserImplementations
        (scan "for (var i = 1;i <= 5;i = i + 1) { print i; }")
        ( Seq.empty
        , "[ { (var i 1.0) (while (<= i 5.0) { { (print i) } (= i (+ i 1.0)) }) } ]")
  describe "programs with function calls" $ do
    testParserImplementations
      (scan "print x(1,2,3*4, y(false)(true), z());")
      ( Seq.empty
      , "[ (print (x 1.0 2.0 (* 3.0 4.0) ((y False) True) (z))) ]")

parserImplementationsAreEquivalent :: QC.Property
parserImplementationsAreEquivalent
  = QC.forAll genTokens $ \tokens ->
      let byTheBookResult = runParser ByTheBook.parse tokens
          megaparsecResult = runParser Megaparsec.parse tokens
      in megaparsecResult `shouldBe` byTheBookResult
  where
    genTokens :: QC.Gen (Seq Token)
    genTokens = do
      tokens <- QC.listOf genToken
                <&> (Token "" Token.EOF Nothing 0 :)
                <&> reverse
      pure . Seq.fromList $ zipWith (\tk line -> tk { tokenLine = line }) tokens [0..]
    genIdentifier :: QC.Gen T.Text
    genIdentifier = QC.elements ["_", "_a", "asd123", "zZz"]
    genToken :: QC.Gen Token
    genToken = do
      tkType <- QC.arbitraryBoundedEnum @Token.TokenType
                `QC.suchThat` (/= Token.EOF)
      (tkLexeme, tkLit) <- case tkType of
        Token.LEFT_PAREN    -> pure ("(", Nothing)
        Token.RIGHT_PAREN   -> pure (")", Nothing)
        Token.LEFT_BRACE    -> pure ("{", Nothing)
        Token.RIGHT_BRACE   -> pure ("}", Nothing)
        Token.COLON         -> pure (":", Nothing)
        Token.COMMA         -> pure (",", Nothing)
        Token.DOT           -> pure (".", Nothing)
        Token.MINUS         -> pure ("-", Nothing)
        Token.PLUS          -> pure ("+", Nothing)
        Token.QUESTION_MARK -> pure ("?", Nothing)
        Token.SEMICOLON     -> pure (";", Nothing)
        Token.SLASH         -> pure ("/", Nothing)
        Token.STAR          -> pure ("*", Nothing)
        Token.BANG          -> pure ("!", Nothing)
        Token.BANG_EQUAL    -> pure ("!=", Nothing)
        Token.EQUAL         -> pure ("=", Nothing)
        Token.EQUAL_EQUAL   -> pure ("==", Nothing)
        Token.GREATER       -> pure (">", Nothing)
        Token.GREATER_EQUAL -> pure (">=", Nothing)
        Token.LESS          -> pure ("<", Nothing)
        Token.LESS_EQUAL    -> pure ("<=", Nothing)
        Token.AND           -> pure ("and", Nothing)
        Token.CLASS         -> pure ("class", Nothing)
        Token.ELSE          -> pure ("else", Nothing)
        Token.FALSE         -> pure ("false", Nothing)
        Token.FUN           -> pure ("fun", Nothing)
        Token.FOR           -> pure ("for", Nothing)
        Token.IF            -> pure ("if", Nothing)
        Token.NIL           -> pure ("nil", Nothing)
        Token.OR            -> pure ("or", Nothing)
        Token.PRINT         -> pure ("print", Nothing)
        Token.RETURN        -> pure ("return", Nothing)
        Token.SUPER         -> pure ("super", Nothing)
        Token.THIS          -> pure ("this", Nothing)
        Token.TRUE          -> pure ("true", Nothing)
        Token.VAR           -> pure ("var", Nothing)
        Token.WHILE         -> pure ("while", Nothing)

        Token.IDENTIFIER    -> genIdentifier <&> (, Nothing)
        Token.STRING -> do
          s <- T.pack <$> QC.arbitrary
          pure ("\"" <> s <> "\"", Just $ Token.LitString s)
        Token.NUMBER -> do
          d <- QC.arbitrary
          pure (T.pack $ show d, Just $ Token.LitNum d)
        _ -> pure ("", Nothing)
      tkLine <- QC.getSize
      pure $ Token tkLexeme tkType tkLit tkLine

testParserImplementations :: Seq Token
                          -> (Seq ParserError, T.Text)
                          -> Spec
testParserImplementations tokens expected = do
  describe "ByTheBook" $
    it "parses correctly" $
      runParser ByTheBook.parse tokens `shouldBe` expected
  describe "Megaparsec" $
    it "parses correctly" $
      runParser Megaparsec.parse tokens `shouldBe` expected

runParser :: _ -> Seq Token -> (Seq ParserError, T.Text)
runParser parse = fmap printAST
                . run
                . Util.runWriterToPair @(Seq ParserError)
                . parse

scan :: T.Text -> Seq Token
scan = run . fmap snd . Util.runWriterToPair @(Seq ScanError) . Scanner.scanTokens
