module Parser where

import           Control.Applicative  (many, (<|>))
import           Control.Effect.State
import           Control.Lens         ((%~), (&))
import           Data.Functor         (($>))
import           Data.List            (foldl')
import           Data.Singletons
import           Data.Text            (Text)
import           Prelude              hiding (pred, succ)

import qualified Command              as C
import           Data.Fin
import           Data.Nat
import qualified Lexer                as L
import qualified Normalize            as N
import qualified Syntax               as S
import           Zoo

file :: Parser L.Token Nat [C.Cmd]
file =   pSingle L.TEOF $> []
     <|> (:) <$> topDirective <*> file
     <|> (:) <$> topDirective <*> (pSingle L.TSemi *> file)
     <|> pure <$> topDef <* pSingle L.TEOF
     <|> (:) <$> topDef <*> (pSingle L.TSemi *> file)
     <|> pure <$> topExpr <* pSingle L.TEOF
     <|> (:) <$> topExpr <*> (pSingle L.TSemi *> file)

commandLine :: Parser L.Token Nat C.Cmd
commandLine =   topDirective <* pSingle L.TEOF
            <|> topDef       <* pSingle L.TEOF
            <|> topExpr      <* pSingle L.TEOF

topDirective :: Parser L.Token Nat C.Cmd
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

name :: Parser L.Token u Text
name = do
  t <- pAny
  case t of
    L.TName n -> pure n
    _         -> pFail

index :: Parser L.Token u Nat
index = do
  t <- pAny
  case t of
    L.TIndex i -> pure $ integerToNat i
    _          -> pFail

topDef :: Parser L.Token Nat C.Cmd
topDef = do
  n <- name
  _ <- pSingle L.TColonEq
  e <- expr
  case e of
    S.MkSomeTerm SZ t -> pure $ C.CDefine n $ S.MkTerm t
    S.MkSomeTerm _  _ -> pFail

topExpr :: Parser L.Token Nat C.Cmd
topExpr = do
  e <- expr
  case e of
    S.MkSomeTerm SZ t -> pure $ C.CExpr $ S.MkTerm t
    S.MkSomeTerm _  _ -> pFail

lamDown :: forall i. Parser i Nat ()
lamDown = MkParser $ modify @(ParserState i Nat) (& #userState %~ succ)

lamUp :: forall i. Parser i Nat ()
lamUp = MkParser $ modify @(ParserState i Nat) (& #userState %~ pred)

expr :: Parser L.Token Nat S.SomeTerm
expr =  pSingle L.TLambda *> lamDown *> (S.unsafeMkLam <$> expr) <* lamUp
    <|> app

app :: Parser L.Token Nat S.SomeTerm
app = do
  ts <- many simple
  case ts of
    []    -> pFail
    t:ts' -> pure $ foldl' S.unsafeMkApp t ts'

simple :: Parser L.Token Nat S.SomeTerm
simple =  pSingle L.TLParen *> expr <* pSingle L.TRParen
      <|> free
      <|> bound

free :: Parser L.Token Nat S.SomeTerm
free = do
  v <- name
  n <- MkParser $ gets @(ParserState L.Token Nat) userState
  pure $ case toSing n of
    SomeSing sn -> S.MkSomeTerm sn $ S.TFree v

bound :: Parser L.Token Nat S.SomeTerm
bound = do
  j <- index
  n <- MkParser $ gets @(ParserState L.Token Nat) userState
  case toSing n of
    SomeSing SZ      -> pFail
    SomeSing (SS n') -> case natToFin n' j of
      Nothing -> pFail
      Just k  -> pure $ S.MkSomeTerm (SS n') $ S.TBound k
