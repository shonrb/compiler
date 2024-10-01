module ParsePass2 where

import Lex
import Ast
import ParseCom
import ParsePass1

import Control.Applicative (Alternative (..), liftA2)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

type MixFixTable = [(Int, [Parser AstExpression])]

data Pass2Context = Pass2Context
  { p2globalSyms :: [String]
  , p2mfparsers  :: MixFixTable
  }

parseInner :: [PrelimTopLevel] -> ParseResult [AstTopLevel]
parseInner d = parseWithContext d $ makeContext d

parseWithContext :: [PrelimTopLevel] -> Pass2Context -> ParseResult [AstTopLevel]
parseWithContext = undefined

-- TODO: Replace empty w builtins 
makeContext :: [PrelimTopLevel] -> Pass2Context
makeContext d = Pass2Context (map getSymbol d) (extractMixFix d M.empty)

extractMixFix :: [PrelimTopLevel] 
              -> M.Map Int [Parser AstExpression]
              -> MixFixTable
extractMixFix [] acc = M.toList acc
extractMixFix (d : ds) acc = extractMixFix ds newAcc where 
  newAcc = case d of
    PrelimPlainFunc p -> acc
    PrelimMixFix m -> M.insert prec newMf acc where
      prec = mfPrecedence m
      newMf = fromMaybe [] $ M.lookup prec acc
  
pMixFix :: MixFix -> Parser AstExpression
pMixFix mf@MixFix{mfParts=ps} = 
  ExprOperation <$> AstOperation (getSymbol mf) <$> pMixFixPart ps

-- TODO: eliminate left recursion
pMixFixPart :: [MixFixPart] -> Parser [AstExpression]
pMixFixPart [] = return []
pMixFixPart (p : ps) = case p of
  MixFixConnector s -> pTok (TId s) *> rest
  MixFixParameter _ -> liftA2 (:) pExpr rest
  where rest = pMixFixPart ps

pExpr :: Parser AstExpression
pExpr = undefined

