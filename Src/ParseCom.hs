module ParseCom where

import Lex

import Control.Monad (ap)
import Control.Applicative (Alternative (..), liftA2)
import Data.Char (isSpace)

type ParseResult a = Either String a

newtype Parser a = Parser
  { parse :: [Token] -> ParseResult (a, [Token])
  }

instance Functor Parser where
  fmap f p = p >>= (return . f)

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  (<*>)  = ap

instance Monad Parser where
  return = pure
  p >>= x = Parser $ \s -> case parse p s of
    Right (a, b) -> parse (x a) b
    Left msg     -> Left msg

instance Alternative Parser where
  empty     = pErr "This shouldn't happen"
  pa <|> pb = Parser $ \s -> case parse pa s of
    Right a -> Right a
    Left _  -> parse pb s

runParser :: Parser a -> [Token] -> ParseResult a
runParser p ts = case parse p ts of
  Right (res, _) -> Right res
  Left err       -> Left err

pErr :: String -> Parser a
pErr msg = Parser $ \s -> Left msg

pAny :: Parser Token
pAny = Parser $ \s -> case s of
  []       -> Left "Unexpected end of file"
  (t : ts) -> Right (t, ts)

pPredicate :: String -> (Token -> Bool) -> Parser Token
pPredicate need pred = do
  t <- pAny 
  if pred t
  then return t
  else pErr $ "Expected " ++ need ++ ", got " ++ show t

pTok :: Token -> Parser Token
pTok t = pPredicate (show t) (==t)

pNotTok :: Token -> Parser Token
pNotTok t = pPredicate ("something other than " ++ show t) (/=t)

pOnePlus :: Parser a -> Parser [a]
pOnePlus p = do
  x <- p
  xs <- pZeroPlus p
  return $ x : xs

pZeroPlus :: Parser a -> Parser [a]
pZeroPlus p = pOnePlus p <|> return []

pOptional :: a -> Parser a -> Parser a
pOptional v p = p <|> return v

pGetInt :: Parser Int
pGetInt = do
  t <- pAny
  case t of 
    TIntLiteral i -> return i
    _             -> pErr "Expected an integer literal"

pGetId :: Parser String
pGetId = do
  t <- pAny
  case t of 
    TId s -> return s
    _     -> pErr "Expected an identifier"

