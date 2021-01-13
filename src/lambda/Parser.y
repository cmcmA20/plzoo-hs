{
module Parser where

import qualified Syntax as S
import qualified Lexer as L
}

%name toplevel Toplevel
%name file File
%error { parseError }
%lexer { L.lexwrap } { L.TEOF }
%monad { L.Alex }
%tokentype { L.Token }

%token
%%

{
parseError :: L.Token -> L.Alex a
parseError _ = do
  (L.AlexPn _ l c,_,_,_) <- L.alexGetInput
  L.alexError $ "parse error at line " <> show l <> ", column " <> show c
}
