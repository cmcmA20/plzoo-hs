{
module Lexer where

import Data.Text (Text)
import Data.Text qualified as T
}

%wrapper "monad"

$alpha = [a-zA-Z]
$digit = 0-9

tokens :-

  $white+                     ;
  $digit+                     { \(_,_,_,s) len -> pure (TNumeral (read (take len s))) }
  $alpha [$alpha $digit \_ ]* { \(_,_,_,s) len -> pure (TVariable (T.pack (take len s))) }
  \=                          { tok TEqual }
  \+                          { tok TPlus }
  \-                          { tok TMinus }
  \*                          { tok TTimes }
  \/                          { tok TDivide }
  \(                          { tok TLParen }
  \)                          { tok TRParen }

{
data Token
  = TNumeral !Integer
  | TVariable !Text
  | TEqual
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
