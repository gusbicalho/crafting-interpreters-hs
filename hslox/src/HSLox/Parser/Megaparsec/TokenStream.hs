module HSLox.Parser.Megaparsec.TokenStream where

import Data.Foldable
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import HSLox.Token (Token (..))
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
