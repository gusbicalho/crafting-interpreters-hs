module HSLox.AST.Sugar where

import qualified Data.Sequence as Seq
import HSLox.AST

buildFor :: Maybe Stmt -> Maybe Expr -> Maybe Expr -> Stmt -> Stmt
buildFor init condition increment body =
    case init of
      Nothing -> whileLoop
      Just init -> blockFromList $ [init, whileLoop]
  where
    blockFromList = BlockStmt . Block . Seq.fromList
    whileLoop = WhileStmt $ While (desugaredCondition condition)
                                  (loopBody increment body)
    desugaredCondition = maybe (BoolE True) id
    loopBody increment body =
      case increment of
        Nothing -> body
        Just increment -> blockFromList $ [body, ExprStmt increment]
