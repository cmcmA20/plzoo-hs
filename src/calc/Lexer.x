{
module Lexer where
}

%wrapper "monad"

$digit = 0-9

tokens :-

  $white+ ;
  \n      { tok TNewline }
  $digit+ { \(_,_,_,s) len -> pure (TNumeral (read (take len s))) }
  \+      { tok TPlus }
  \-      { tok TMinus }
  \*      { tok TTimes }
  \/      { tok TDivide }
  \(      { tok TLParen }
  \)      { tok TRParen }

{
data Token
  = TNewline
  | TNumeral !Integer
  | TPlus
  | TMinus
  | TTimes
  | TDivide
  | TLParen
  | TRParen
  | TEOF
  deriving (Eq, Show)

tok :: Token -> AlexInput -> Int -> Alex Token
tok t (_, _, _, _) _ = pure t

alexEOF :: Alex Token
alexEOF = pure TEOF

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)
}
