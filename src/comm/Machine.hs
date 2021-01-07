{-|
  A simple machine with a program, RAM, program counter and a stack pointer. The program
  is an array of instructions. The stack grows downwards. Arithmetical and boolean
  operations operate on the stack.
-}
module Machine where

import           Control.Carrier.State.Strict
import           Control.Carrier.Throw.Either
import           Control.Exception (Exception)
import           Control.Lens
import           Control.Monad (when)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import           GHC.Generics (Generic)

import Zoo

-- | The machine instructions.
data Instruction
  = INOP -- ^ no operation
  | ISET !Int -- ^ pop from stack and store in the given location
  | IGET !Int -- ^ push from given location onto stack
  | IPUSH !Int -- ^ push integer constant onto stack
  | IADD -- ^ addition
  | ISUB -- ^ subtraction
  | IMUL -- ^ multiplication
  | IDIV -- ^ division
  | IMOD -- ^ remainder
  | IEQ -- ^ equal
  | ILT -- ^ less than
  | IAND -- ^ conjunction
  | IOR -- ^ disjunction
  | INOT -- ^ negation
  | IJMP !Int -- ^ relative jump
  | IJMPZ !Int -- ^ relative jump if zero on the stack
  | IPRINT -- ^ pop from stack and print
  deriving (Eq, Show)

-- | Machine errors.
data MachineError
  = MEIllegalAddress
  | MEIllegalInstruction
  | MEZeroDivision

instance Show MachineError where
  show MEIllegalAddress     = "illegal address"
  show MEIllegalInstruction = "illegal instruction"
  show MEZeroDivision       = "division by zero"

instance Exception MachineError

-- | The state of the machine
data MachineState = MkMachineState
  { code :: !(IntMap Instruction) -- ^ program
  , ram  :: !(IntMap Int) -- ^ memory
  , pc   :: !Int -- ^ program counter
  , sp   :: !Int } -- ^ stack pointer
  deriving (Generic, Show)

make :: IntMap Instruction -> Int -> MachineState
make code ramSize = MkMachineState
  { code
  , ram = IM.fromList $ zip [1..] $ replicate ramSize 0
  , pc = 0
  , sp = ramSize - 1
  }

readMem :: (Has (State MachineState) sig m, Has (Throw MachineError) sig m) => Int -> m Int
readMem k = gets (IM.lookup k . ram) >>= maybeThrow MEIllegalAddress

writeMem :: (Has (State MachineState) sig m, Has (Throw MachineError) sig m) => Int -> Int -> m ()
writeMem k x = do
  st <- get @MachineState
  if k >= IM.size (st ^. #ram)
     then throwError MEIllegalAddress
     else put $ st & #ram %~ IM.insert k x

popStack :: (Has (State MachineState) sig m, Has (Throw MachineError) sig m) => m Int
popStack = do
  s <- gets @MachineState sp
  x <- readMem s
  modify @MachineState (& #sp +~ 1)
  pure x

pushStack :: (Has (State MachineState) sig m, Has (Throw MachineError) sig m) => Int -> m ()
pushStack x = do
  s <- gets @MachineState sp
  modify @MachineState (& #sp -~ 1)
  writeMem s x

b2i :: Bool -> Int
b2i False = 0
b2i True  = 1

executeInstruction :: (Has (State MachineState) sig m, Has (Throw MachineError) sig m) => Instruction -> m ((), RuntimeAction)
executeInstruction INOP = pure ((), RANop)
executeInstruction (ISET k) = do
  x <- popStack
  writeMem k x
  pure ((), RANop)
executeInstruction (IGET k) = do
  x <- readMem k
  pushStack x
  pure ((), RANop)
executeInstruction (IPUSH x) = pushStack x >> pure ((), RANop)
executeInstruction IADD = do
  y <- popStack
  x <- popStack
  pushStack $ x + y
  pure ((), RANop)
executeInstruction ISUB = do
  y <- popStack
  x <- popStack
  pushStack $ x - y
  pure ((), RANop)
executeInstruction IMUL = do
  y <- popStack
  x <- popStack
  pushStack $ x * y
  pure ((), RANop)
executeInstruction IDIV = do
  y <- popStack
  x <- popStack
  if y == 0
     then throwError MEZeroDivision
     else pushStack $ x `div` y
  pure ((), RANop)
executeInstruction IMOD = do
  y <- popStack
  x <- popStack
  if y == 0
     then throwError MEZeroDivision
     else pushStack $ x `mod` y
  pure ((), RANop)
executeInstruction IEQ = do
  y <- popStack
  x <- popStack
  pushStack $ b2i $ x == y
  pure ((), RANop)
executeInstruction ILT = do
  y <- popStack
  x <- popStack
  pushStack $ b2i $ x < y
  pure ((), RANop)
executeInstruction IAND = do
  y <- popStack
  x <- popStack
  pushStack $ b2i $ x /= 0 && y /= 0
  pure ((), RANop)
executeInstruction IOR = do
  y <- popStack
  x <- popStack
  pushStack $ b2i $ x /= 0 || y /= 0
  pure ((), RANop)
executeInstruction INOT = do
  x <- popStack
  pushStack $ b2i $ x == 0
  pure ((), RANop)
executeInstruction (IJMP k) = modify @MachineState (& #pc +~ k) >> pure ((), RANop)
executeInstruction (IJMPZ k) = do
  x <- popStack
  when (x == 0) $ modify @MachineState (& #pc +~ k)
  pure ((), RANop)
executeInstruction IPRINT = do
  x <- popStack
  pure ((), RAPrint (T.pack (show x) <> "\n"))

-- TODO
runProgram :: (Has (State MachineState) sig m, Has (Throw MachineError) sig m) => m ()
runProgram = do
  st <- get @MachineState
  undefined
