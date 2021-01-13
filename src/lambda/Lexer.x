{
module Lexer where

import           Data.Text (Text)
import qualified Data.Text as T
}

%wrapper "monad"

$alpha  = [a-zA-Z]
$digit  = 0-9
$symbol = [\' \! \@ \$ \% \& \* \- \+ \| \\ \[ \] \{ \} \, \< \= \> \? \/ \~ \`]

@name    = [$alpha $digit $symbol]+
@index   = \_ [$digit]+
@comment = "--" .* \n

tokens :-

  @comment    ;
  \n          ;
  [\  \r \t]+ ;
  @name       { \(_,_,_,s) len -> pure (TName (T.pack (take len s))) }
  @index      { \(_,_,_,s) len -> pure (TIndex (read (take len s))) }
  ":context"  { tok TContext }
  ":help"     { tok THelp }
  ":quit"     { tok TQuit }
  ":constant" { tok TConstant }
  ":eager"    { tok TEager }
  ":lazy"     { tok TLazy }
  ":deep"     { tok TDeep }
  ":shallow"  { tok TShallow }
  \(          { tok TLParen }
  \)          { tok TRParen }
  ":="        { tok TColonEq }
  [\^ \λ]     { tok TLambda }
  \;          { tok TSemi }

{
data Token
  = TName !Text
  | TIndex !Integer
  | TContext
  | THelp
  | TQuit
  | TConstant
  | TEager
  | TLazy
  | TDeep
  | TShallow
  | TLParen
  | TRParen
  | TColonEq
  | TLambda
  | TSemi
  | TEOF
  deriving (Eq, Show)

tok :: Token -> AlexInput -> Int -> Alex Token
tok t (_, _, _, _) _ = pure t

alexEOF :: Alex Token
alexEOF = pure TEOF

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)
}
