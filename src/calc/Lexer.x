{
module Lexer where
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+ ;
  \n      { tok TNewline }
  $digit+ { \(p,_,_,s) _ -> pure (TNumeral (read s)) }
  \+      { tok TPlus }
  \-      { tok TMinus }
  \*      { tok TTimes }
  \/      { tok TDivide }
  \(      { tok TLParen }
  \)      { tok TRParen }

{
data Token
  = TNewline
  | TNumeral Integer
  | TPlus
  | TMinus
  | TTimes
  | TDivide
  | TLParen
  | TRParen
  | TEOF
  deriving (Eq, Show)

tok :: Token -> AlexInput -> Int -> Alex Token
tok t (p, _, _, s) _ = pure t

alexEOF :: Alex Token
alexEOF = pure TEOF

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)
}
