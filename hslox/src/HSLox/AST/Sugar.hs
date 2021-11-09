module HSLox.AST.Sugar where

import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import HSLox.AST qualified as AST

buildFor ::
  Maybe AST.StmtI ->
  Maybe AST.ExprI ->
  Maybe AST.ExprI ->
  AST.StmtI ->
  AST.StmtI
buildFor init condition increment body =
  case init of
    Nothing -> whileLoop
    Just init -> blockFromList [init, whileLoop]
 where
  blockFromList = AST.BlockStmtI . AST.Block . Seq.fromList
  whileLoop =
    AST.WhileStmtI $
      AST.While
        (desugaredCondition condition)
        (loopBody increment body)
  desugaredCondition = fromMaybe (AST.BoolE True)
  loopBody increment body =
    case increment of
      Nothing -> body
      Just increment -> blockFromList [body, AST.ExprStmtI increment]
