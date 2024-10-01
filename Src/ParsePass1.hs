module ParsePass1 
  ( MixFixPart (..)
  , MixFix (..)
  , PlainFunc (..)
  , PrelimTopLevel (..)
  , Symbol (..)
  , parseTopLevel
  )
  where

import ParseCom
import Lex

import Control.Applicative (Alternative (..), liftA2)

class Symbol a where
  getSymbol :: a -> String

data MixFixPart 
  = MixFixConnector String
  | MixFixParameter [Token]
  deriving Show

data MixFix = MixFix 
  { mfParts      :: [MixFixPart]
  , mfPrecedence :: Int
  , mfReturns    :: Maybe [Token]
  , mfBody       :: [Token]
  }
  deriving Show

data PlainFunc = PlainFunc
  { pfName       :: String
  , pfParameters :: [[Token]]
  , pfReturns    :: Maybe [Token]
  , pfBody       :: [Token]
  } 
  deriving Show

data PrelimTopLevel 
  = PrelimPlainFunc PlainFunc
  | PrelimMixFix    MixFix
  deriving Show

instance Symbol PlainFunc where
  getSymbol PlainFunc{pfName=n} = n 

instance Symbol MixFix where
  getSymbol MixFix{mfParts=parts} = collapse parts where
    collapse [] = []
    collapse (p : ps) = (case p of 
      MixFixConnector s -> s
      MixFixParameter _ -> "{}"
      ) ++ collapse ps

instance Symbol PrelimTopLevel where
    getSymbol (PrelimPlainFunc p) = getSymbol p
    getSymbol (PrelimMixFix m) = getSymbol m

parseTopLevel :: [Token] -> ParseResult [PrelimTopLevel]
parseTopLevel = runParser $ pAll

pAll :: Parser [PrelimTopLevel]
pAll = return [] <* pTok TEOF <|> liftA2 (:) pTop pAll

pTop :: Parser PrelimTopLevel
pTop = do
  t <- pAny
  case t of 
    TDefFunction -> PrelimPlainFunc <$> pDefFunction 
    TDefOperator -> PrelimMixFix <$> pDefOperator
    _            -> pErr $ "Unknown top-level declaration: " ++ show t

pDefFunction :: Parser PlainFunc
pDefFunction = do
  name <- pGetId
  params <- pOnePlus pFuncParam
  (rt, body) <- pFuncCommon 
  return $ PlainFunc name params rt body

pDefOperator :: Parser MixFix
pDefOperator = do
  precedence <- pGetInt
  parts <- pOnePlus pMixFix
  (rt, body) <- pFuncCommon 
  return $ MixFix parts precedence rt body

pFuncCommon :: Parser (Maybe [Token], [Token])
pFuncCommon = do
  rt <- pReturnType
  pTok TDefBodyStart
  body <- pExpr "function body"
  pTok TDefEnd
  return (rt, body)

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

