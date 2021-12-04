-- | Abstract syntax.
module Language.CalcVar.Syntax where

import Data.Text (Text)

-- | Arithmetical expressions.
data Exp
  = Variable !Text -- ^ a variable
  | Numeral !Integer -- ^ non-negative integer constant
  | Plus !Exp !Exp -- ^ addition [e1 + e2]
  | Minus !Exp !Exp -- ^ difference [e1 - e2]
  | Times !Exp !Exp -- ^ product [e1 * e2]
  | Divide !Exp !Exp -- ^ quotient [e1 / e2]
  | Negate !Exp -- ^ opposite value [-e]
  deriving (Eq, Show)

-- | Toplevel commands are expressions and definitions.
data Cmd
  = Expression !Exp
  | Definition !Text !Exp
  deriving (Eq, Show)
