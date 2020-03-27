module HSLox.Scanner.ByTheBook.ScanLoop where

import Prelude hiding (getLine)
import Control.Carrier.Empty.Church
import Control.Carrier.State.Church
import Control.Carrier.Writer.Church
import Control.Effect.Sum (Members, (:+:))
import Control.Monad (forever)
import Data.Bool (bool)
import Data.Char (isDigit, isLetter)
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import HSLox.Scanner.ByTheBook.ScanState
  ( ScanState
  , initialScanState
  , resetSegment
  -- , getSegment
  -- , getLine
  -- , incLine
  -- , advance
  -- , peek
  -- , peek2
  -- , match
  , isAtEnd
  )
import HSLox.TreeWalk.Error (Error (..))
import qualified HSLox.Util as Util

-- type ScanNext error tk = forall sig m . Has Empty sig m
--                                      => Has (State [error]) sig m
--                                      => Has (State ScanState) sig m
--                                      => m tk

-- type BuildEOF tk = forall sig m. Has (State ScanState) sig m
--                               => m tk

-- scanTokens :: forall error tk sig m.
--                 Has (State [error]) sig m
--               => ScanNext error tk
--               -> BuildEOF tk
--               -> T.Text
--               -> m (Seq tk)
-- scanTokens scanNextToken buildEOFToken source
--     = evalState (initialScanState source)
--     . execWriter @(Seq tk)
--     $ do
--       Util.runEmptyToMaybe . forever $ do
--         resetSegment
--         maybeToken <- Util.runEmptyToMaybe scanNextToken
--         Util.whenJust maybeToken $ do
--           addToken
--         guard =<< isAtEnd
--       addToken =<< buildEOFToken
--   where
--     addToken token = tell $ Seq.singleton token
