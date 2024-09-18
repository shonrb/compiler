module Lex where

import Data.Char

data Token 
  = TId String
  | TIntLiteral Int
  | TFloatLiteral Float
  | TStrLiteral String
  | TCharLiteral Char
  | TOpenBracket 
  | TCloseBracket 
  | TOpenCurly 
  | TCloseCurly 
  | TDefFunction
  | TDefOperator
  | TDefMapTo
  | TDefBodyStart
  | TDefEnd
  | TComma
  | TEquals
  | TEOF
  | TError String
  deriving (Show, Eq)

lexAll :: String -> [Token]
lexAll [] = [TEOF]
lexAll str@(c : cs)
  | isSpace c             = lexAll cs
  | c == '('              = TOpenBracket  : lexAll cs
  | c == ')'              = TCloseBracket : lexAll cs
  | c == '{'              = TOpenCurly  : lexAll cs
  | c == '}'              = TCloseCurly : lexAll cs
  | c == '#'              = lexComment cs
  | c == '"'              = lexStrLiteral cs
  | c == '\''             = lexCharLiteral cs
  | isDigit c             = lexNumber str
  | isSymChar c           = lexSymbol str
  | isAlpha c || c == '_' = lexIdent str
  | otherwise             = TError "Unrecognised Character" : lexAll cs

lexComment :: String -> [Token]
lexComment = lexAll . dropWhile (/='\n')

lexNumber :: String -> [Token]
lexNumber = lexN (\c -> isWordChar c || c == '.') $ \s -> TIntLiteral 0 -- TODO: Number parsing

lexSymbol :: String -> [Token]
lexSymbol = lexN isSymChar makeSym where
  makeSym "->" = TDefMapTo
  makeSym ";"  = TDefEnd
  makeSym s    = TId s

lexIdent :: String -> [Token]
lexIdent = lexN isWordChar makeWord where
  makeWord "function" = TDefFunction
  makeWord "operator" = TDefOperator
  makeWord "is"       = TDefBodyStart
  makeWord s          = TId s

lexN :: (Char -> Bool) -> (String -> Token) -> String -> [Token]
lexN included make s = make (takeWhile included s) : lexAll (dropWhile included s)

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_' 

isSymChar :: Char -> Bool
isSymChar c = (isSymbol c || isPunctuation c) && not (elem c "(){}#_" )

lexStrLiteral :: String -> [Token]
lexStrLiteral = lexLiteral TStrLiteral '"' []

lexCharLiteral :: String -> [Token]
lexCharLiteral = lexLiteral makeChar '\'' [] where
  makeChar s 
    | length s == 1 = TCharLiteral $ head s 
    | otherwise     = TError "Invalid character literal"

lexLiteral :: (String -> Token) -> Char -> String -> String -> [Token]
lexLiteral mk delim acc ('\\' : c : cs) = case lexEscape c of
  Just n  -> lexLiteral mk delim (acc ++ [n]) cs
  Nothing -> TError "Invalid Escape Sequence" : []
lexLiteral mk delim acc (c : cs) 
  | c == delim = mk acc : lexAll cs
  | otherwise  = lexLiteral mk delim (acc ++ [c]) cs
lexLiteral mk delim acc [] = TError "Unterminated Literal" : []

lexEscape :: Char -> Maybe Char
lexEscape '\\' = Just '\\'
lexEscape 'n'  = Just '\n'
lexEscape 't'  = Just '\t'
lexEscape '"'  = Just '"'
lexEscape '\'' = Just '\''
lexEscape _    = Nothing



