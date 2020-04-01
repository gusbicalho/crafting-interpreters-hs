module HSLox.Parser.ByTheBook.Parser where

import Control.Carrier.Lift
import Control.Carrier.State.Church
import Control.Carrier.Trace.Printing
import qualified Data.Text as T
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import HSLox.AST
import HSLox.Error
import HSLox.Parser.ByTheBook.ParserState
import HSLox.Token (Token (..))
import qualified HSLox.Token as Token
import qualified HSLox.Util as Util

parseTokens ::
  forall sig m. Has (State [Error]) sig m
             => Has Trace sig m
             => (Seq Token)
             -> m Expr
parseTokens tokens = pure NilE

testParser = runM @IO
           . runTrace
           . runState @[Error] (\s a -> pure (s,a)) []
           . parseTokens
