{
module Parser where

import qualified Syntax as S
import qualified Lexer as L
}

%name program Program
%name file File
%error { parseError }
%lexer { L.lexwrap } { L.TEOF }
%monad { L.Alex }
%tokentype { L.Token }

%token
num    { L.TNumeral $$  }
var    { L.TVariable $$ }
lparen { L.TLParen      }
rparen { L.TRParen      }
equal  { L.TEqual       }
less   { L.TLess        }
plus   { L.TPlus        }
minus  { L.TMinus       }
times  { L.TTimes       }
divide { L.TDivide      }
rem    { L.TRemainder   }
true   { L.TTrue        }
false  { L.TFalse       }
and    { L.TAnd         }
or     { L.TOr          }
not    { L.TNot         }
new    { L.TNew         }
in     { L.TIn          }
skip   { L.TSkip        }
if     { L.TIf          }
then   { L.TThen        }
else   { L.TElse        }
end    { L.TEnd         }
while  { L.TWhile       }
do     { L.TDo          }
done   { L.TDone        }
print  { L.TPrint       }
read   { L.TRead        }
assign { L.TAssign      }
semi   { L.TSemicolon   }

%nonassoc in
%left semi
%left or
%left and
%nonassoc not
%left plus minus
%left times divide rem

%%

File :: { [S.Cmd] }
  : Program { [$1] }

Program :: { S.Cmd }
  : Command { $1 }

Command :: { S.Cmd }
  : skip { S.CSkip }
  | new var assign ArExp in Command { S.CNew $2 $4 $6 }
  | print ArExp { S.CPrint $2 }
  | read var { S.CRead $2 }
  | var assign ArExp { S.CAssign $1 $3 }
  | Command semi Command { S.CSeq $1 $3 }
  | while BoolExp do Command done { S.CWhile $2 $4 }
  | if BoolExp then Command else Command end { S.CCond $2 $4 $6 }
  | lparen Command rparen { $2 }

ArExp :: { S.ArExp }
  : var { S.AEVariable $1 }
  | num { S.AENumeral $1 }
  | ArExp times ArExp { S.AETimes $1 $3 }
  | ArExp plus ArExp { S.AEPlus $1 $3 }
  | ArExp minus ArExp { S.AEMinus $1 $3 }
  | ArExp divide ArExp { S.AEDivide $1 $3 }
  | ArExp rem ArExp { S.AERemainder $1 $3 }
  | lparen ArExp rparen { $2 }

BoolExp :: { S.BoolExp }
  : true { S.BETrue }
  | false { S.BEFalse }
  | ArExp equal ArExp { S.BEEqual $1 $3 }
  | ArExp less ArExp { S.BELess $1 $3 }
  | BoolExp and BoolExp { S.BEAnd $1 $3 }
  | BoolExp or BoolExp { S.BEOr $1 $3 }
  | not BoolExp { S.BENot $2 }
  | lparen BoolExp rparen { $2 }

{
parseError :: L.Token -> L.Alex a
parseError _ = do
  (L.AlexPn _ l c,_,_,_) <- L.alexGetInput
  L.alexError $ "parse error at line " <> show l <> ", column " <> show c
}
