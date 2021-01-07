{
module Lexer where

import           Data.Text (Text)
import qualified Data.Text as T
}

%wrapper "monad"

$alpha = [a-zA-Z]
$digit = 0-9

@variable = [\_ $alpha ] [\_ $alpha $digit]*
@number = \-? [$digit]+

tokens :-

  $white+   ;
  \n        { tok TNewline }
  @number   { \(_,_,_,s) len -> pure (TNumeral (read (take len s))) }
  "true"    { tok TTrue }
  "false"   { tok TFalse }
  "skip"    { tok TSkip }
  "if"      { tok TIf }
  "then"    { tok TThen }
  "else"    { tok TElse }
  "end"     { tok TEnd }
  "while"   { tok TWhile }
  "do"      { tok TDo }
  "done"    { tok TDone }
  "print"   { tok TPrint }
  "new"     { tok TNew }
  "in"      { tok TIn }
  "and"     { tok TAnd }
  "or"      { tok TOr }
  "not"     { tok TNot }
  ":="      { tok TAssign }
  \;        { tok TSemicolon }
  \(        { tok TLParen }
  \)        { tok TRParen }
  \+        { tok TPlus }
  \-        { tok TMinus }
  \*        { tok TTimes }
  \/        { tok TDivide }
  \%        { tok TRemainder }
  \=        { tok TEqual }
  \<        { tok TLess }
  @variable { \(_,_,_,s) len -> pure (TVariable (T.pack (take len s))) }

{
data Token
  = TNewline
  | TNumeral !Integer
  | TTrue
  | TFalse
  | TSkip
  | TIf
  | TThen
  | TElse
  | TEnd
  | TWhile
  | TDo
  | TDone
  | TPrint
  | TNew
  | TIn
  | TAnd
  | TOr
  | TNot
  | TAssign
  | TSemicolon
  | TLParen
  | TRParen
  | TPlus
  | TMinus
  | TTimes
  | TDivide
  | TRemainder
  | TEqual
  | TLess
  | TVariable !Text
  | TEOF
  deriving (Eq, Show)

tok :: Token -> AlexInput -> Int -> Alex Token
tok t (_, _, _, _) _ = pure t

alexEOF :: Alex Token
alexEOF = pure TEOF

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)
}
