import Lex
import Ast
import ParsePass1
import ParsePass2

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "An input file is required"
    (file : []) -> do
      contents <- readFile file
      print (parseTopLevel (lexAll contents))
    _ -> putStrLn "Too many input files"

