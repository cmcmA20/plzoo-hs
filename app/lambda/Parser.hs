module Parser where

import Control.Applicative ((<|>))
import Data.Text (Text)

import qualified Command   as C
import qualified Lexer     as L
import qualified Normalize as N
import qualified Syntax    as S
import           Zoo

commandLine :: Parser L.Token () C.Cmd
commandLine =   topDirective
            <|> topDef
            <|> topExpr

topDirective :: Parser L.Token () C.Cmd
topDirective = do
  t <- pAny
  case t of
    L.TEager    -> pure $ C.CEnergy N.EEager
    L.TLazy     -> pure $ C.CEnergy N.ELazy
    L.TDeep     -> pure $ C.CDepth N.DDeep
    L.TShallow  -> pure $ C.CDepth N.DShallow
    L.TContext  -> pure C.CContext
    L.THelp     -> pure C.CHelp
    L.TQuit     -> pure C.CQuit
    L.TConstant -> C.CConst <$> name
    _           -> pFail

name :: Parser L.Token () Text
name = do
  t <- pAny
  case t of
    L.TName n -> pure n
    _         -> pFail

topDef :: Parser L.Token () C.Cmd
topDef = do
  n <- name
  _ <- pSingle L.TColonEq
  e <- expr
  pure $ C.CDefine n (S.MkTerm e)

-- (^x) y

topExpr :: Parser L.Token () C.Cmd
topExpr = C.CExpr . S.MkTerm <$> expr

expr :: Parser L.Token () (S.Term' n)
expr =   pSingle L.TLambda *> (S.TLam <$> expr)
     <|> app

app :: Parser L.Token () (S.Term' n)
app =   simple
    <|> S.TApp <$> app <*> simple

simple :: Parser L.Token () (S.Term' n)
simple =   var
       <|> pSingle L.TLParen *> expr <* pSingle L.TRParen

var :: Parser L.Token () (S.Term' n)
var = free <|> bound

free :: Parser L.Token () (S.Term' n)
free = S.TFree <$> name

bound :: Parser L.Token () (S.Term' n)
bound = do
  t <- pAny
  case t of
    L.TIndex i -> undefined -- FIXME
    _          -> pFail


file :: Parser L.Token () [C.Cmd]
file = undefined -- FIXME
