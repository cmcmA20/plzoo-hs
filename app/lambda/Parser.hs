module Parser where

import Control.Effect.Reader
import Control.Effect.State
import Control.Effect.Throw
import Data.Kind (Type)
import Data.Text (Text)

import qualified Command   as C
import qualified Lexer     as L
import qualified Normalize as N
import           Zoo

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
    _           -> undefined

name :: Parser L.Token () Text
name = do
  t <- pAny
  case t of
    L.TName n -> pure n
    _         -> undefined

-- topDef :: Parser L.Token () C.Cmd
-- topDef = do
--   n <- name
--   _ <- sendM @(LamParser m) $ single L.TColonEq
--   e <- expr
--   pure $ C.CDefine n e
--
-- type LamParser m = ParsecT SyntaxError [L.Token] m
-- 
-- type W sig m =
--   ( Has (Reader Integer) sig m
--   , Has (Lift (LamParser m)) sig m
--   , Alternative m )
-- 
-- file :: W sig m => m [C.Cmd]
-- file = undefined
-- 
-- commandLine :: W sig m => m C.Cmd
-- commandLine = topDef <|> topExpr <|> topDirective
-- 
-- topExpr :: W sig m => m C.Cmd
-- topExpr = C.CExpr <$> expr
-- 
-- 
-- name :: W sig m => m Text
-- name = pure undefined
-- 
-- expr :: W sig m => m S.Term
-- expr = pure undefined

-- parseError :: L.Token -> L.Alex a
-- parseError _ = do
--   (L.AlexPn _ l c,_,_,_) <- L.alexGetInput
--   L.alexError $ "parse error at line " <> show l <> ", column " <> show c
