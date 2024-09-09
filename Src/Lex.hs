module Lex where

import Data.Char

data Token 
  = Id String
  | IntLiteral Int
  | FloatLiteral Float
  | StrLiteral String
  | CharLiteral Char
  | OpenBracket 
  | CloseBracket 
  | EndLine
  | Comma
  | Error String
  deriving (Show, Eq)

lexAll :: String -> [Token]
lexAll [] = []
lexAll str@(c : cs)
  | elem c singles        = lexSingle c : lexAll cs
  | isSpace c             = lexAll cs
  | c == '#'              = lexComment cs
  | c == '"'              = lexStrLiteral cs
  | c == '\''             = lexCharLiteral cs
  | isDigit c || c == '.' = lexNumber str
  | otherwise             = lexIdent str

singles :: String 
singles = "();,"

lexSingle :: Char -> Token
lexSingle '(' = OpenBracket
lexSingle ')' = CloseBracket
lexSingle ';' = EndLine
lexSingle ',' = Comma

lexComment :: String -> [Token]
lexComment = lexAll . dropWhile (/='\n')

lexNumber :: String -> [Token]
lexNumber = lexWord $ \s -> IntLiteral 0 -- TODO: Number parsing

lexIdent :: String -> [Token]
lexIdent = lexWord Id

lexWord :: (String -> Token) -> String -> [Token]
lexWord mk s = mk (takeWhile isWord s) : lexAll (dropWhile isWord s)
  where isWord c = not $ isSpace c || elem c singles || elem c "\"'#"

lexStrLiteral :: String -> [Token]
lexStrLiteral = lexLiteral StrLiteral '"' []

lexCharLiteral :: String -> [Token]
lexCharLiteral = lexLiteral makeChar '\'' [] where
  makeChar s 
    | length s == 1 = CharLiteral $ head s 
    | otherwise     = Error "Invalid character literal"

lexLiteral :: (String -> Token) -> Char -> String -> String -> [Token]
lexLiteral mk delim acc ('\\' : c : cs) = case lexEscape c of
  Just n  -> lexLiteral mk delim (acc ++ [n]) cs
  Nothing -> Error "Invalid Escape Sequence" : []
lexLiteral mk delim acc (c : cs) 
  | c == delim = mk acc : lexAll cs
  | otherwise  = lexLiteral mk delim (acc ++ [c]) cs
lexLiteral mk delim acc [] = Error "Unterminated Literal" : []

lexEscape :: Char -> Maybe Char
lexEscape '\\' = Just '\\'
lexEscape 'n'  = Just '\n'
lexEscape 't'  = Just '\t'
lexEscape '"'  = Just '"'
lexEscape '\'' = Just '\''
lexEscape _    = Nothing



