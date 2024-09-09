import Lex

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "An input file is required"
    (file : []) -> do
      contents <- readFile file
      mapM_ print $ lexAll contents
    _ -> putStrLn "Too many input files"

