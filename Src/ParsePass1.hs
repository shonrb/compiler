module ParsePass1 where

import ParseCom
import Lex

import Debug.Trace

import Control.Applicative (Alternative (..), liftA2)

data FunctionCommon = FunctionCommon
  { id         :: String
  , returnType :: Maybe [Token]
  , body       :: [Token]
  }
  deriving Show

data MixFixPart 
  = MixFixConnector String
  | MixFixParameter [Token]
  deriving Show

data MixFix = MixFix 
  { parts      :: [MixFixPart]
  , precedence :: Int
  , opRest     :: FunctionCommon
  }
  deriving Show

data PlainFunction = PlainFunction
  { parameters :: [[Token]]
  , funcRest   :: FunctionCommon       
  } 
  deriving Show

data Definition 
  = DefFunction PlainFunction
  | DefMixFix   MixFix
  deriving Show

parseTopLevel :: [Token] -> Either String [Definition]
parseTopLevel = runParser $ pAll

pAll :: Parser [Definition]
pAll = (return [] <* pTok TEOF) <|> (liftA2 (:) pTop pAll)

pTop :: Parser Definition
pTop = do
  t <- pAny
  case t of 
    TDefFunction -> pDefFunction 
    TDefOperator -> pDefOperator
    _            -> pErr $ "Unknown top-level declaration: " ++ show t

pDefFunction :: Parser Definition
pDefFunction = do
  name <- pGetId
  params <- pOnePlus pFuncParam
  rest <- pFunctionCommon ""
  return $ DefFunction (PlainFunction params rest)

pDefOperator :: Parser Definition
pDefOperator = do
  precedence <- pGetInt
  parts <- pOnePlus pMixFix
  rest <- pFunctionCommon ""
  return $ DefMixFix (MixFix parts precedence rest)

pFunctionCommon :: String -> Parser FunctionCommon
pFunctionCommon id = do
  rt <- pReturnType
  pTok TDefBodyStart
  body <- pExpr "function body"
  pTok TDefEnd
  return $ FunctionCommon id rt body

pReturnType :: Parser (Maybe [Token])
pReturnType = pOptional Nothing $ Just <$> (pTok TDefMapTo *> pExpr "type declaration")

pPrecedence :: Parser Int
pPrecedence = pTok TOpenBracket *> pGetInt <* pTok TCloseBracket <|> return 10

pMixFix :: Parser MixFixPart
pMixFix = pMixFixParam <|> pMixFixOp <|> pErr "Invalid operator definition"

pMixFixParam :: Parser MixFixPart
pMixFixParam = MixFixParameter <$> pFuncParam

pMixFixOp :: Parser MixFixPart
pMixFixOp = MixFixConnector <$> pGetId

pFuncParam :: Parser [Token]
pFuncParam = pTok TOpenCurly *> pExpr "parameter declaration" <* pTok TCloseCurly

pExpr :: String -> Parser [Token]
pExpr what = pOnePlus (pPredicate what (\t -> not $ elem t reserved)) where
  reserved = 
    [ TDefFunction
    , TDefOperator
    , TDefEnd
    , TDefBodyStart
    , TDefMapTo
    , TOpenCurly
    , TCloseCurly ]

