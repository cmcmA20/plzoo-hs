module Parser where

import Control.Applicative
import Control.Effect.Lift
import Control.Effect.Reader
import Data.Text (Text)

import qualified Command as C
import qualified Lexer as L
import qualified Normalize as N
import qualified Syntax as S
import           Zoo

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
-- topDef :: forall sig m. W sig m => m C.Cmd
-- topDef = do
--   n <- name
--   _ <- sendM @(LamParser m) $ single L.TColonEq
--   e <- expr
--   pure $ C.CDefine n e
-- 
-- topDirective :: forall sig m. W sig m => m C.Cmd
-- topDirective = do
--   t <- sendM @(LamParser m) anySingle
--   case t of
--     L.TEager -> pure $ C.CEnergy N.EEager
--     L.TLazy -> pure $ C.CEnergy N.ELazy
--     L.TDeep -> pure $ C.CDepth N.DDeep
--     L.TShallow -> pure $ C.CDepth N.DShallow
--     L.TContext -> pure C.CContext
--     L.THelp -> pure C.CHelp
--     L.TQuit -> pure C.CQuit
--     L.TConstant -> C.CConst <$> name
--     _ -> undefined
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
