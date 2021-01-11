-- | Abstract syntax.
module Syntax where

-- | Arithmetical expressions.
data Exp
  = Numeral !Integer -- ^ non-negative integer constant
  | Plus !Exp !Exp -- ^ addition [e1 + e2]
  | Minus !Exp !Exp -- ^ difference [e1 - e2]
  | Times !Exp !Exp -- ^ product [e1 * e2]
  | Divide !Exp !Exp -- ^ quotient [e1 / e2]
  | Negate !Exp -- ^ opposite value [-e]
  deriving (Eq, Show)

type Cmd = Exp
