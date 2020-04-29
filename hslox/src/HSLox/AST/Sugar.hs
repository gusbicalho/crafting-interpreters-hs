module HSLox.AST.Sugar where

import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import HSLox.AST

buildFor :: Maybe StmtI
         -> Maybe ExprI
         -> Maybe ExprI
         -> StmtI
         -> StmtI
buildFor init condition increment body =
    case init of
      Nothing -> whileLoop
      Just init -> blockFromList [init, whileLoop]
  where
    blockFromList = BlockStmtI . Block . Seq.fromList
    whileLoop = WhileStmtI $ While (desugaredCondition condition)
                                   (loopBody increment body)
    desugaredCondition = fromMaybe (BoolE True)
    loopBody increment body =
      case increment of
        Nothing -> body
        Just increment -> blockFromList [body, ExprStmtI increment]
