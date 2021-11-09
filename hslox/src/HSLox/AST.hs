{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module HSLox.AST where

import Data.Sequence (Seq)
import Data.Text qualified as T
import HSLox.AST.Meta (WithMeta, pattern NoMeta)
import HSLox.Token (Token (..))

type ProgramI = Program ()
type StmtI = Stmt ()
type ExprI = Expr ()

newtype Program meta = Program (Seq (Stmt meta))
deriving stock instance (Show (Stmt meta)) => Show (Program meta)

data Stmt meta
  = ExprStmt (WithMeta meta (Expr meta))
  | VarDeclarationStmt (WithMeta meta (VarDeclaration meta))
  | FunDeclarationStmt (WithMeta meta (FunDeclaration meta))
  | ClassDeclarationStmt (WithMeta meta (ClassDeclaration meta))
  | BlockStmt (WithMeta meta (Block meta))
  | IfStmt (WithMeta meta (If meta))
  | WhileStmt (WithMeta meta (While meta))
  | ReturnStmt (WithMeta meta (Return meta))
deriving stock instance
  ( Show (WithMeta meta (Expr meta))
  , Show (WithMeta meta (VarDeclaration meta))
  , Show (WithMeta meta (FunDeclaration meta))
  , Show (WithMeta meta (ClassDeclaration meta))
  , Show (WithMeta meta (Block meta))
  , Show (WithMeta meta (If meta))
  , Show (WithMeta meta (While meta))
  , Show (WithMeta meta (Return meta))
  ) =>
  Show (Stmt meta)

pattern ExprStmtI :: ExprI -> StmtI
pattern ExprStmtI expr = ExprStmt (NoMeta expr)

pattern VarDeclarationStmtI :: VarDeclaration () -> StmtI
pattern VarDeclarationStmtI expr = VarDeclarationStmt (NoMeta expr)

pattern FunDeclarationStmtI :: FunDeclaration () -> StmtI
pattern FunDeclarationStmtI expr = FunDeclarationStmt (NoMeta expr)

pattern ClassDeclarationStmtI :: ClassDeclaration () -> StmtI
pattern ClassDeclarationStmtI expr = ClassDeclarationStmt (NoMeta expr)

pattern BlockStmtI :: Block () -> StmtI
pattern BlockStmtI expr = BlockStmt (NoMeta expr)

pattern IfStmtI :: If () -> StmtI
pattern IfStmtI expr = IfStmt (NoMeta expr)

pattern WhileStmtI :: While () -> StmtI
pattern WhileStmtI expr = WhileStmt (NoMeta expr)

pattern ReturnStmtI :: Return () -> StmtI
pattern ReturnStmtI expr = ReturnStmt (NoMeta expr)

data VarDeclaration meta = VarDeclaration
  { varDeclarationIdentifier :: Token
  , varDeclarationInitializer :: Expr meta
  }
deriving stock instance (Show (Expr meta)) => Show (VarDeclaration meta)

data FunDeclaration meta = FunDeclaration
  { funDeclarationIdentifier :: Token
  , funDeclarationInitializer :: Function meta
  }
deriving stock instance (Show (Function meta)) => Show (FunDeclaration meta)

data ClassDeclaration meta = ClassDeclaration
  { classDeclarationIdentifier :: Token
  , classDeclarationSuperclass :: Maybe (WithMeta meta Variable)
  , classDeclarationMethods :: Seq (WithMeta meta (Function meta))
  }
deriving stock instance
  ( Show (WithMeta meta (Function meta))
  , Show (WithMeta meta Variable)
  ) =>
  Show (ClassDeclaration meta)

newtype Block meta = Block {blockBody :: Seq (Stmt meta)}
deriving stock instance (Show (Stmt meta)) => Show (Block meta)

data If meta = If
  { ifCondition :: Expr meta
  , ifThenStmt :: Stmt meta
  , ifElseStmt :: Maybe (Stmt meta)
  }
deriving stock instance (Show (Stmt meta), Show (Expr meta)) => Show (If meta)

data While meta = While
  { whileCondition :: Expr meta
  , whileBody :: Stmt meta
  }
deriving stock instance (Show (Stmt meta), Show (Expr meta)) => Show (While meta)

data Return meta = Return
  { returnToken :: Token
  , returnValue :: Maybe (Expr meta)
  }
deriving stock instance (Show (Expr meta)) => Show (Return meta)

pattern UnaryExprI :: Unary () -> Expr ()
pattern UnaryExprI e = UnaryExpr (NoMeta e)
pattern UnaryE :: Token -> ExprI -> ExprI
pattern UnaryE op expr = UnaryExpr (NoMeta (Unary op expr))

pattern LogicalExprI :: Logical () -> Expr ()
pattern LogicalExprI e = LogicalExpr (NoMeta e)
pattern LogicalE :: ExprI -> Token -> ExprI -> ExprI
pattern LogicalE left op right = LogicalExpr (NoMeta (Logical left op right))

pattern BinaryExprI :: Binary () -> Expr ()
pattern BinaryExprI e = BinaryExpr (NoMeta e)
pattern BinaryE :: ExprI -> Token -> ExprI -> ExprI
pattern BinaryE left op right = BinaryExpr (NoMeta (Binary left op right))

pattern TernaryExprI :: Ternary () -> Expr ()
pattern TernaryExprI e = TernaryExpr (NoMeta e)
pattern TernaryE :: ExprI -> Token -> ExprI -> Token -> ExprI -> ExprI
pattern TernaryE left op1 middle op2 right = TernaryExpr (NoMeta (Ternary left op1 middle op2 right))

pattern GroupingExprI :: Grouping () -> Expr ()
pattern GroupingExprI e = GroupingExpr (NoMeta e)
pattern GroupingE :: ExprI -> ExprI
pattern GroupingE expr = GroupingExpr (NoMeta (Grouping expr))

pattern VariableExprI :: Variable -> Expr ()
pattern VariableExprI e = VariableExpr (NoMeta e)
pattern VariableE :: Token -> ExprI
pattern VariableE tk = VariableExpr (NoMeta (Variable tk))

pattern AssignmentExprI :: Assignment () -> Expr ()
pattern AssignmentExprI e = AssignmentExpr (NoMeta e)
pattern AssignmentE :: Token -> ExprI -> ExprI
pattern AssignmentE tk expr = AssignmentExpr (NoMeta (Assignment tk expr))

pattern CallExprI :: Call () -> Expr ()
pattern CallExprI e = CallExpr (NoMeta e)
pattern CallE :: ExprI -> Token -> Seq ExprI -> ExprI
pattern CallE callee paren args = CallExpr (NoMeta (Call callee paren args))

pattern GetPropertyExprI :: GetProperty () -> Expr ()
pattern GetPropertyExprI e = GetPropertyExpr (NoMeta e)
pattern GetPropertyE :: ExprI -> Token -> ExprI
pattern GetPropertyE object name = GetPropertyExpr (NoMeta (GetProperty object name))

pattern SetPropertyExprI :: SetProperty () -> Expr ()
pattern SetPropertyExprI e = SetPropertyExpr (NoMeta e)
pattern SetPropertyE :: ExprI -> Token -> ExprI -> ExprI
pattern SetPropertyE object name value = SetPropertyExpr (NoMeta (SetProperty object name value))

pattern ThisE :: Token -> ExprI
pattern ThisE tk = ThisExpr (NoMeta (This tk))

pattern SuperE :: Token -> Token -> ExprI
pattern SuperE keyword property = SuperExpr (NoMeta (Super keyword property))

pattern LiteralExprI :: Literal -> Expr ()
pattern LiteralExprI e = LiteralExpr (NoMeta e)

pattern StringE :: T.Text -> ExprI
pattern StringE t = LiteralExpr (NoMeta (LitString t))

pattern NumE :: Double -> ExprI
pattern NumE v = LiteralExpr (NoMeta (LitNum v))

pattern BoolE :: Bool -> ExprI
pattern BoolE v = LiteralExpr (NoMeta (LitBool v))

pattern NilE :: ExprI
pattern NilE = LiteralExpr (NoMeta LitNil)

pattern FunctionExprI :: Function () -> Expr ()
pattern FunctionExprI e = FunctionExpr (NoMeta e)
pattern FunctionE :: Token -> Maybe Token -> Seq Token -> Block () -> ExprI
pattern FunctionE funMarker funRecId args body = FunctionExpr (NoMeta (Function funMarker funRecId args body))

data Expr meta
  = UnaryExpr (WithMeta meta (Unary meta))
  | LogicalExpr (WithMeta meta (Logical meta))
  | BinaryExpr (WithMeta meta (Binary meta))
  | TernaryExpr (WithMeta meta (Ternary meta))
  | GroupingExpr (WithMeta meta (Grouping meta))
  | LiteralExpr (WithMeta meta Literal)
  | VariableExpr (WithMeta meta Variable)
  | AssignmentExpr (WithMeta meta (Assignment meta))
  | CallExpr (WithMeta meta (Call meta))
  | GetPropertyExpr (WithMeta meta (GetProperty meta))
  | SetPropertyExpr (WithMeta meta (SetProperty meta))
  | ThisExpr (WithMeta meta This)
  | SuperExpr (WithMeta meta Super)
  | FunctionExpr (WithMeta meta (Function meta))

deriving stock instance
  ( Show (WithMeta meta (Unary meta))
  , Show (WithMeta meta (Logical meta))
  , Show (WithMeta meta (Binary meta))
  , Show (WithMeta meta (Ternary meta))
  , Show (WithMeta meta (Grouping meta))
  , Show (WithMeta meta Literal)
  , Show (WithMeta meta Variable)
  , Show (WithMeta meta (Assignment meta))
  , Show (WithMeta meta (Call meta))
  , Show (WithMeta meta (GetProperty meta))
  , Show (WithMeta meta (SetProperty meta))
  , Show (WithMeta meta This)
  , Show (WithMeta meta Super)
  , Show (WithMeta meta (Function meta))
  ) =>
  Show (Expr meta)

data Ternary meta = Ternary
  { ternaryLeft :: Expr meta
  , ternaryFirstOperator :: Token
  , ternaryMiddle :: Expr meta
  , ternarySecondOperator :: Token
  , ternaryRight :: Expr meta
  }
deriving stock instance (Show (Expr meta)) => Show (Ternary meta)

data Binary meta = Binary
  { binaryLeft :: Expr meta
  , binaryOperator :: Token
  , binaryRight :: Expr meta
  }
deriving stock instance (Show (Expr meta)) => Show (Binary meta)

data Logical meta = Logical
  { logicalLeft :: Expr meta
  , logicalOperator :: Token
  , logicalRight :: Expr meta
  }
deriving stock instance (Show (Expr meta)) => Show (Logical meta)

data Unary meta = Unary
  { unaryOperator :: Token
  , unaryRight :: Expr meta
  }
deriving stock instance (Show (Expr meta)) => Show (Unary meta)

newtype Grouping meta = Grouping {groupingExpr :: Expr meta}
deriving stock instance (Show (Expr meta)) => Show (Grouping meta)

data Function meta = Function
  { functionMarker :: Token
  , functionRecursiveIdentifier :: Maybe Token
  , functionArgs :: Seq Token
  , functionBody :: Block meta
  }
deriving stock instance (Show (Block meta)) => Show (Function meta)

data Literal
  = LitString T.Text
  | LitNum Double
  | LitBool Bool
  | LitNil
  deriving stock (Eq, Show, Ord)

newtype Variable = Variable {variableIdentifier :: Token}
  deriving stock (Eq, Show, Ord)

data Assignment meta = Assignment
  { assignmentIdentifier :: Token
  , assignmentRValue :: Expr meta
  }
deriving stock instance (Show (Expr meta)) => Show (Assignment meta)

data Call meta = Call
  { callCallee :: Expr meta
  , callParen :: Token
  , callArguments :: Seq (Expr meta)
  }
deriving stock instance (Show (Expr meta)) => Show (Call meta)

data GetProperty meta = GetProperty
  { getPropertyObject :: Expr meta
  , getPropertyProperty :: Token
  }
deriving stock instance (Show (Expr meta)) => Show (GetProperty meta)

data SetProperty meta = SetProperty
  { setPropertyObject :: Expr meta
  , setPropertyProperty :: Token
  , setPropertyValue :: Expr meta
  }
deriving stock instance (Show (Expr meta)) => Show (SetProperty meta)

newtype This = This {thisToken :: Token}
  deriving stock (Eq, Show)

data Super = Super
  { superKeyword :: Token
  , superProperty :: Token
  }
  deriving stock (Eq, Show)
