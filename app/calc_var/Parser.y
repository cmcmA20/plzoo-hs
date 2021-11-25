{
module Parser where

import Syntax qualified as S
import Lexer qualified as L
}

%name toplevel Toplevel
%name expression Exp
%error { parseError }
%lexer { L.lexwrap } { L.TEOF }
%monad { L.Alex }
%tokentype { L.Token }

%token
num    { L.TNumeral $$  }
var    { L.TVariable $$ }
equal  { L.TEqual       }
plus   { L.TPlus        }
minus  { L.TMinus       }
times  { L.TTimes       }
divide { L.TDivide      }
lparen { L.TLParen      }
rparen { L.TRParen      }

%left plus minus
%left times divide
%nonassoc uminus

%%

Toplevel :: {S.Cmd }
  : Exp {S.Expression $1 }
  | var equal Exp {S.Definition $1 $3 }

Exp :: { S.Exp }
  : var { S.Variable $1 }
  | num { S.Numeral $1 }
  | Exp times Exp { S.Times $1 $3 }
  | Exp plus Exp { S.Plus $1 $3 }
  | Exp minus Exp { S.Minus $1 $3 }
  | Exp divide Exp { S.Divide $1 $3 }
  | minus Exp %prec uminus { S.Negate $2 }
  | lparen Exp rparen { $2 }

{
parseError :: L.Token -> L.Alex a
parseError _ = do
  (L.AlexPn _ l c,_,_,_) <- L.alexGetInput
  L.alexError $ "parse error at line " <> show l <> ", column " <> show c
}
