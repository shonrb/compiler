module Ast where

data AstOperation = AstOperation
  { opId :: String
  , args :: [AstExpression]
  }
  deriving Show

data AstExpression 
  = ExprId String
  | ExprOperation AstOperation
  deriving Show

data AstFunc = AstFunc
  { funcName :: String
  , params :: [AstExpression]
  , returnType :: Maybe AstExpression
  , body :: AstExpression
  }
  deriving Show

data AstTopLevel
  = AstTopFuncDef AstFunc

