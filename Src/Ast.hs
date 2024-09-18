module Ast where

data AstOperation = AstOperation
  { opId :: String
  , args :: [AstExpression]
  }

data AstExpression 
  = ExprId String
  | ExprOperation 



