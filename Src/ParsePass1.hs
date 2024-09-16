module ParsePass1 where

import ParseCom
import Lex

import Debug.Trace

import Control.Applicative (Alternative (..), liftA2)

data OperatorPart 
  = DefPartConnector String
  | DefPartParameter [Token]
  deriving Show

data Definition = Definition 
  { id         :: String
  , parts      :: [OperatorPart]
  , precedence :: Int
  , returnType :: Maybe [Token]
  , body       :: [Token]
  }
  deriving Show

dbg :: Show a => a -> a
dbg a = traceShow a a

parseDefinitionShapes :: [Token] -> Either String [Definition]
parseDefinitionShapes = runParser $ pZeroPlus pDefinition

pDefinition :: Parser Definition
pDefinition = do
  pTok TStartDef 
  prec <- pPrecedence
  parts <- pOnePlus pPart
  rt <- pReturnType
  pTok TDefBodyStart
  body <- pExpr "function body"
  pTok TEndDef
  return $ Definition "" parts prec rt body

pReturnType :: Parser (Maybe [Token])
pReturnType = pOptional Nothing $ Just <$> (pTok TMapTo *> pExpr "type declaration")

pPrecedence :: Parser Int
pPrecedence = pTok TOpenBracket *> pGetInt <* pTok TCloseBracket <|> return 10

pPart :: Parser OperatorPart
pPart = pPartArg <|> pPartOp <|> pErr "Invalid operator definition"

pPartArg :: Parser OperatorPart
pPartArg = DefPartParameter <$> (pTok TOpenCurly *> pExpr "argument declaration" <* pTok TCloseCurly)

pPartOp :: Parser OperatorPart
pPartOp = DefPartConnector <$> pGetId

pExpr :: String -> Parser [Token]
pExpr what = pOnePlus (pPredicate what (\t -> not $ elem t reserved))
  where reserved = [TStartDef, TEndDef, TDefBodyStart, TMapTo, TOpenCurly, TCloseCurly]



