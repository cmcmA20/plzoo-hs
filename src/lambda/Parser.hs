module Parser where

import Text.Parsec

import qualified Syntax as S
import qualified Lexer as L

type LamParser m = ParsecT [L.Token] Integer m S.Term

-- parseError :: L.Token -> L.Alex a
-- parseError _ = do
--   (L.AlexPn _ l c,_,_,_) <- L.alexGetInput
--   L.alexError $ "parse error at line " <> show l <> ", column " <> show c
