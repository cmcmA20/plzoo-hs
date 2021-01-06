-- Abstract syntax.
module Syntax where

import Data.Text (Text)

-- | Arithmetical expressions.
data Exp
  = Variable !Text      -- a variable
  | Numeral  !Integer   -- non-negative integer constant
  | Plus     !Exp !Exp  -- Addition [e1 + e2]
  | Minus    !Exp !Exp  -- Difference [e1 - e2]
  | Times    !Exp !Exp  -- Product [e1 * e2]
  | Divide   !Exp !Exp  -- Quotient [e1 / e2]
  | Negate   !Exp       -- Opposite value [-e]
  deriving (Eq, Show)

-- | Toplevel commands are expressions and definitions.
data Cmd
  = Expression !Exp
  | Definition !Text !Exp
