{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module HSLox.ParserSpec where

import Control.Carrier.Lift
import Data.Functor
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
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
      ( Set.empty
      , "[ ]"
      )
  describe "expression without identifiers and keywords" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "1 / 2 / 3 + (2 * 4) == 9 + 6 ? !!true : -false, 9 < 11;")
        ( Set.empty
        , "[ (, (?: (== (+ (/ (/ 1.0 2.0) 3.0) (group (* 2.0 4.0))) (+ 9.0 6.0)) (! (! True)) (- False)) (< 9.0 11.0)) ]"
        )
    describe "with error productions" $ do
      testParserImplementations
        (scan "1 / 2, < 11; \n+2+2; \n-1-1; \n*-4; == 7")
        ( Set.fromList
            [ ParserError (Just $ Token "<"  Token.LESS        Nothing 1) "Binary operator < found at the beginning of expression."
            , ParserError (Just $ Token "+"  Token.PLUS        Nothing 2) "Binary operator + found at the beginning of expression."
            , ParserError (Just $ Token "*"  Token.STAR        Nothing 4) "Binary operator * found at the beginning of expression."
            , ParserError (Just $ Token "==" Token.EQUAL_EQUAL Nothing 4) "Binary operator == found at the beginning of expression."
            ]
        , "[ (- (- 1.0) 1.0) ]"
        )
  describe "programs with function calls" $ do
    testParserImplementations
      (scan "print(x(1,2,3*4, y(false)(true), z()));")
      ( Set.empty
      , "[ (print (x 1.0 2.0 (* 3.0 4.0) ((y False) True) (z))) ]")
  describe "programs with expression and declaration statements, identifier expressions and assignment" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "120 / 2; print(123 + 4 * 7); var x = 2 + 3; var y = 7; print(x+y); x = y = 9; print(x*y);")
        ( Set.empty
        , "[ (/ 120.0 2.0) (print (+ 123.0 (* 4.0 7.0))) (var x (+ 2.0 3.0)) (var y 7.0) (print (+ x y)) (= x (= y 9.0)) (print (* x y)) ]"
        )
  describe "programs with blocks" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "var x = 120 / 2; print(x); { var x = 7; print(x); x = 3; print(x); {} } print(x);")
        ( Set.empty
        , "[ (var x (/ 120.0 2.0)) (print x) { (var x 7.0) (print x) (= x 3.0) (print x) { } } (print x) ]"
        )
    describe "with unterminated block" $ do
      testParserImplementations
        (scan "var x = 120 / 2; print(x); { var x = 7; print(x); { var y = 7; }")
        ( Set.fromList
            [ ParserError (Just $ Token "" Token.EOF Nothing 1) "Expect '}' after block."
            ]
        , "[ (var x (/ 120.0 2.0)) (print x) ]"
        )
    describe "with nested unterminated blocks" $ do
      testParserImplementations
        (scan "{ { } {")
        ( Set.fromList
            [ ParserError (Just $ Token "" Token.EOF Nothing 1) "Expect '}' after block."
            ]
        , "[ ]"
        )
    describe "with unterminated statement inside block" $ do
      testParserImplementations
        (scan "var x = 120 / 2; print(x); { var x = 7 }")
        ( Set.fromList
            [ ParserError (Just $ Token "}" Token.RIGHT_BRACE Nothing 1) "Expect ';' after variable declaration."
            ]
        , "[ (var x (/ 120.0 2.0)) (print x) ]"
        )
  describe "programs with if statements" $ do
    describe "correct and nested" $ do
      testParserImplementations
        (scan "if (!(true == false)) if (false) print(1); else print(2); else { if (true) { print(5); } else if (false) print(7); }")
        ( Set.empty
        , "[ (if (! (group (== True False))) (if False (print 1.0) (print 2.0)) { (if True { (print 5.0) } (if False (print 7.0))) }) ]"
        )
    describe "dangling else" $ do
      testParserImplementations
        (scan "if (first) if (second) print(1); else print(2);")
        ( Set.empty
        , "[ (if first (if second (print 1.0) (print 2.0))) ]"
        )
    describe "incomplete ifs" $ do
      testParserImplementations
        (scan "if;\n if (;\n if (true;\n if (true) else;\n if (true) {} else;\n if (true) else {};\n {}")
        ( Set.fromList
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
        (scan "if (false or true and false) print(1); else print(2);")
        ( Set.empty
        , "[ (if (or False (and True False)) (print 1.0) (print 2.0)) ]")
  describe "programs with while statements" $ do
    describe "correct" $ do
      testParserImplementations
        (scan "var x = 0; while (x < 5) { print(x); x = x + 1; }")
        ( Set.empty
        , "[ (var x 0.0) (while (< x 5.0) { (print x) (= x (+ x 1.0)) }) ]")
  describe "programs with for statements" $ do
    describe "loop forever" $ do
      testParserImplementations
        (scan "for (;;) print(1);")
        ( Set.empty
        , "[ (while True (print 1.0)) ]")
    describe "count to 5" $ do
      testParserImplementations
        (scan "for (var i = 1;i <= 5;i = i + 1) { print(i); }")
        ( Set.empty
        , "[ { (var i 1.0) (while (<= i 5.0) { { (print i) } (= i (+ i 1.0)) }) } ]")
  describe "programs with function declarations" $ do
    describe "without returns" $ do
      testParserImplementations
        (scan "fun printTwo(x, y) { print(x); print(y); } printTwo(3,4);")
        ( Set.empty
        , "[ (fun printTwo [x y] { (print x) (print y) }) (printTwo 3.0 4.0) ]")
    describe "with return values" $ do
      testParserImplementations
        (scan "fun square(x) { return x*x; } print(square(3));")
        ( Set.empty
        , "[ (fun square [x] { (return (* x x)) }) (print (square 3.0)) ]")
  describe "programs with anonymous fn expressions" $ do
    testParserImplementations
      (scan "fun twice(f) { return fun (a) { f(a); f(a); }; } twice(print)(3);")
      ( Set.empty
      , "[ (fun twice [f] { (return (fun [a] { (f a) (f a) })) }) ((twice print) 3.0) ]")
  describe "using return as assignment target" $ do
    testParserImplementations
      (scan "return true = nil;")
      ( Set.fromList
          [ ParserError (Just $ Token "=" Token.EQUAL Nothing 1) "Invalid assignment target."
          ]
      , "[ (return True) ]")
  describe "invalid assignment target, missing ; at end" $ do
    testParserImplementations
      (scan "-24 = -33 nil")
      ( Set.fromList
          [ ParserError (Just $ Token "=" Token.EQUAL Nothing 1) "Invalid assignment target."
          , ParserError (Just $ Token "nil" Token.NIL Nothing 1) "Expect ';' after expression."]
      , "[ ]")
  describe "programs with classes" $ do
    testParserImplementations
      (scan "class Klass { one() { print(1); } two(a) { print(a); } }")
      ( Set.empty
      , "[ (class Klass (one [] { (print 1.0) }) (two [a] { (print a) })) ]")
  describe "programs with property access" $ do
    testParserImplementations
      (scan "class Klass { one() { print(1); } } var a = Klass(); a.one(); a.two = 2;")
      ( Set.empty
      , "[ (class Klass (one [] { (print 1.0) })) (var a (Klass)) ((.one a)) (.two= a 2.0) ]")
  describe "programs with `this`" $ do
    testParserImplementations
      (scan "class A { play() { print(this.song); }} var a = A(); a.song = \"The Lemon Song\"; var play = a.play; play();")
      ( Set.empty
      , "[ (class A (play [] { (print (.song this)) })) (var a (A)) (.song= a \"The Lemon Song\") (var play (.play a)) (play) ]")
  describe "programs with classes extending classes" $ do
    testParserImplementations
      (scan "class Klass { one() { print(1); } } class Qlass < Klass { two(a) { print(a); } }")
      ( Set.empty
      , "[ (class Klass (one [] { (print 1.0) })) (class Qlass < Klass (two [a] { (print a) })) ]")

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
                          -> (Set ParserError, T.Text)
                          -> Spec
testParserImplementations tokens expected = do
  describe "ByTheBook" $
    it "parses correctly" $
      runParser ByTheBook.parse tokens `shouldBe` expected
  describe "Megaparsec" $
    it "parses correctly" $
      runParser Megaparsec.parse tokens `shouldBe` expected

runParser :: _ -> Seq Token -> (Set ParserError, T.Text)
runParser parse = fmap printAST
                . run
                . Util.runWriterToPair @(Set ParserError)
                . parse

scan :: T.Text -> Seq Token
scan = run . fmap snd . Util.runWriterToPair @(Set ScanError) . Scanner.scanTokens
