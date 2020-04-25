module HSLox.Parser.Megaparsec.TokenStream where


import Data.Foldable
import Data.Sequence (Seq (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import HSLox.Token (Token (..))
import Text.Megaparsec (PosState (..), SourcePos (..), mkPos)
import qualified Text.Megaparsec.Stream as Stream

newtype TokenStream = TokenStream { unTokenStream :: Seq Token }

instance Stream.Stream TokenStream where
  type Token TokenStream = Token
  type Tokens TokenStream = (Seq Token)

  tokenToChunk _ = Seq.singleton

  tokensToChunk _ = Seq.fromList

  chunkToTokens _ = toList

  chunkLength _ = Seq.length

  chunkEmpty _ Seq.Empty = True
  chunkEmpty _ _ = True

  take1_ (TokenStream (tk :<| tks)) = Just (tk, TokenStream tks)
  take1_ _ = Nothing

  takeN_ n
    | n <= 0 = \s -> Just (Seq.empty, s)
    | otherwise = \case
      (TokenStream Empty) -> Nothing
      (TokenStream s) ->
        let (front, back) = Seq.splitAt n s
        in Just (front, TokenStream back)

  takeWhile_ pred (TokenStream seq) =
    let (front, back) = Seq.spanl pred seq
    in (front, TokenStream back)

  showTokens _ (tk :| []) = T.unpack (tokenLexeme tk)
  showTokens _ (tk :| tks)
    = T.unpack $ tokenLexeme tk
              <> snd (foldl' appendTk (tokenLine tk, "") tks)
    where
      appendTk (prevLine, s) tk
        | prevLine == tokenLine tk = (tokenLine tk, s <> " "  <> tokenLexeme tk)
        | otherwise                = (tokenLine tk, s <> "\n" <> tokenLexeme tk)

  tokensLength p tks = length (Stream.showTokens p tks)

  reachOffset offset posSt@PosState { pstateInput
                                    , pstateOffset
                                    , pstateSourcePos
                                    }
    = ( lineText
      , posSt { pstateInput = TokenStream back
              , pstateOffset = offset
              , pstateSourcePos =
                pstateSourcePos { sourceLine = finalLineNum
                                , sourceColumn = mkPos 1
                                }
              })
    where
      (front, back) = Seq.splitAt (offset - pstateOffset) . unTokenStream $ pstateInput
      finalLineNum = case front of
                      _ :|> tk -> mkPos $ tokenLine tk
                      _ -> case back of
                        tk :<| _ -> mkPos $ tokenLine tk
                        _ -> sourceLine pstateSourcePos
      lineText = "<empty line>"
