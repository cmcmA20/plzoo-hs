-- | Abstract syntax.
module Syntax where

import           Data.Text (Text)

-- | Arithmetical expressions.
data ArExp
  = AEVariable !Text -- ^ a variable
  | AENumeral !Int -- ^ integer constant
  | AEPlus !ArExp !ArExp -- ^ addition [e1 + e2]
  | AEMinus !ArExp !ArExp -- ^ difference [e1 - e2]
  | AETimes !ArExp !ArExp -- ^ product [e1 * e2]
  | AEDivide !ArExp !ArExp -- ^ quotient [e1 / e2]
  | AERemainder !ArExp !ArExp -- ^ remainder [e1 % e2]
  deriving (Eq, Show)

-- | Boolean expressions.
data BoolExp
  = BETrue -- ^ constant [true]
  | BEFalse -- ^ constant [false]
  | BEEqual !ArExp !ArExp -- ^ equal [e1 = e2]
  | BELess !ArExp !ArExp -- ^ less than [e1 < e2]
  | BEAnd !BoolExp !BoolExp -- ^ conjunction [b1 and b2]
  | BEOr !BoolExp !BoolExp -- ^ disjunction [b1 or b2]
  | BENot !BoolExp -- ^ negation [not b]
  deriving (Eq, Show)

-- | Commands.
data Cmd
  = CSkip -- ^ no operation [skip]
  | CNew !Text !ArExp !Cmd -- ^ variable declaration [new x := e in c]
  | CPrint !ArExp -- ^ print expression [print e]
  | CRead !Text -- ^ read expression into variable [read x]
  | CAssign !Text !ArExp -- ^ assign a variable [x := e]
  | CSeq !Cmd !Cmd -- ^ sequence commands [c1 ; c2]
  | CWhile !BoolExp !Cmd -- ^ loop [while b do c done]
  | CCond !BoolExp !Cmd !Cmd -- ^ conditional [if b then c1 else c2 end]
  deriving (Eq, Show)
