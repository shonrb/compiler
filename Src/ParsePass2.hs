module ParsePass2 where

import Lex
import Ast
import ParseCom
import ParsePass1

import Control.Applicative (Alternative (..), liftA2)
import Data.Map

data Pass2Context = Pass2Context
  { p2mfparsers  :: Map Int [Parser AstExpression]
  , p2globalSyms :: [String]
  }

parseInner :: [PrelimTopLevel] -> ParseResult [AstTopLevel]
parseInner = undefined

makeContext :: [PrelimTopLevel] -> ParseResult Pass2Context
makeContext = undefined

pMixFix :: MixFix -> Parser AstExpression
pMixFix mf@MixFix{mfParts=ps} = 
  ExprOperation <$> AstOperation (getSymbol mf) <$> pMixFixPart ps

pMixFixPart :: [MixFixPart] -> Parser [AstExpression]
pMixFixPart [] = return []
pMixFixPart (p : ps) = case p of
  MixFixConnector s -> pTok (TId s) *> rest
  MixFixParameter _ -> liftA2 (:) pExpr rest
  where rest = pMixFixPart ps

pExpr :: Parser AstExpression
pExpr = undefined

