module Parser where

import Control.Applicative ((<|>), many)
-- import Control.Effect.State
-- import Control.Lens hiding (simple)
import Control.Monad (join)
import Data.Functor (($>))
import Data.List (foldl1')
import Data.Text (Text)

import qualified Command   as C
import qualified Lexer     as L
import qualified Normalize as N
import qualified Syntax    as S
import           Zoo

file :: Parser L.Token () [C.Cmd]
file =   pSingle L.TEOF $> []
     <|> pure <$> topDirective <* pSingle L.TEOF
     <|> (:) <$> topDirective <*> (file <* pSingle L.TSemi) <* pSingle L.TEOF
     <|> pure <$> topDef <* pSingle L.TEOF
     <|> (:) <$> topDef <*> (file <* pSingle L.TSemi) <* pSingle L.TEOF
     <|> pure <$> topExpr <* pSingle L.TEOF
     <|> (:) <$> topExpr <*> (file <* pSingle L.TSemi) <* pSingle L.TEOF

commandLine :: Parser L.Token () C.Cmd
commandLine =   topDirective <* pSingle L.TEOF
            <|> topDef       <* pSingle L.TEOF
            <|> topExpr      <* pSingle L.TEOF

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

topExpr :: Parser L.Token () C.Cmd
topExpr = C.CExpr . S.MkTerm <$> expr

expr :: Parser L.Token () (S.Term' n)
expr =   pSingle L.TLambda *> (S.TLam <$> expr)
     <|> foldl1' S.TApp . join <$> many app -- FIXME awfulness ensues

-- lamExpandCtx :: Parser L.Token () (S.Term' ('S.S n)) -> Parser L.Token Integer (S.Term' n)
-- lamExpandCtx p = MkParser $ modify @(ParserState L.Token ()) (& #userState +~ 1)

app :: Parser L.Token () [S.Term' n]
app =   pure <$> simple
    <|> (:) <$> simple <*> app

simple :: Parser L.Token () (S.Term' n)
simple =   pSingle L.TLParen *> expr <* pSingle L.TRParen
       <|> var

var :: Parser L.Token () (S.Term' n)
var = free <|> bound

free :: Parser L.Token () (S.Term' n)
free = S.TFree <$> name

bound :: Parser L.Token () (S.Term' n)
bound = do
  t <- pAny
  case t of
    L.TIndex i -> undefined -- pure $ S.TBound $ S.toFin i
    _          -> pFail
