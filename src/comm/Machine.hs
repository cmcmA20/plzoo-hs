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
import           Text.Read (readMaybe)

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
  | IREAD -- ^ read and push on stack
  deriving (Eq, Generic, Show)

-- | Machine errors.
data MachineError
  = MEIllegalAddress
  | MEIllegalInstruction
  | MEZeroDivision
  | MEInputOutput

instance Show MachineError where
  show MEIllegalAddress     = "illegal address"
  show MEIllegalInstruction = "illegal instruction"
  show MEZeroDivision       = "division by zero"
  show MEInputOutput        = "I/O"

instance Exception MachineError

-- | The state of the machine
data MachineState = MkMachineState
  { code :: !(IntMap Instruction) -- ^ program
  , ram  :: !(IntMap Int) -- ^ memory
  , pc   :: !Int -- ^ program counter
  , sp   :: !Int } -- ^ stack pointer
  deriving (Generic, Show)

type Machine sig m =
  ( Has (State MachineState) sig m
  , Has (Throw MachineError) sig m
  , Has Runtime sig m)

type Sem = ()

type Ctx = ()

mkMachineState :: [Instruction] -> Int -> MachineState
mkMachineState prog ramSize = MkMachineState
  { code = IM.fromList $ zip [0..] prog
  , ram = IM.fromList $ zip [0..] $ replicate ramSize 0
  , pc = 0
  , sp = ramSize - 1 }

readMem :: Machine sig m => Int -> m Int
readMem k = gets (IM.lookup k . ram) >>= maybeThrow MEIllegalAddress

writeMem :: Machine sig m => Int -> Int -> m ()
writeMem k x = do
  st <- get @MachineState
  if k < 0 || k >= IM.size (st ^. #ram)
     then throwError MEIllegalAddress
     else put $ st & #ram %~ IM.insert k x

popStack :: Machine sig m => m Int
popStack = do
  oldSP <- gets @MachineState sp
  x <- readMem oldSP
  let newSP = oldSP + 1
  modify @MachineState (& #sp .~ newSP)
  pure x

pushStack :: Machine sig m => Int -> m ()
pushStack x = do
  oldSP <- gets @MachineState sp
  let newSP = oldSP - 1
  modify @MachineState (& #sp .~ newSP)
  writeMem newSP x

b2i :: Bool -> Int
b2i False = 0
b2i True  = 1

executeInstruction :: Machine sig m => Instruction -> m Sem
executeInstruction INOP = pure ()
executeInstruction (ISET k) = do
  x <- popStack
  writeMem k x
executeInstruction (IGET k) = do
  x <- readMem k
  pushStack x
executeInstruction (IPUSH x) = pushStack x >> pure mempty
executeInstruction IADD = do
  y <- popStack
  x <- popStack
  pushStack $ x + y
executeInstruction ISUB = do
  y <- popStack
  x <- popStack
  pushStack $ x - y
executeInstruction IMUL = do
  y <- popStack
  x <- popStack
  pushStack $ x * y
executeInstruction IDIV = do
  y <- popStack
  x <- popStack
  if y == 0
     then throwError MEZeroDivision
     else pushStack $ x `div` y
executeInstruction IMOD = do
  y <- popStack
  x <- popStack
  if y == 0
     then throwError MEZeroDivision
     else pushStack $ x `mod` y
executeInstruction IEQ = do
  y <- popStack
  x <- popStack
  pushStack $ b2i $ x == y
executeInstruction ILT = do
  y <- popStack
  x <- popStack
  pushStack $ b2i $ x < y
executeInstruction IAND = do
  y <- popStack
  x <- popStack
  pushStack $ b2i $ x /= 0 && y /= 0
executeInstruction IOR = do
  y <- popStack
  x <- popStack
  pushStack $ b2i $ x /= 0 || y /= 0
executeInstruction INOT = do
  x <- popStack
  pushStack $ b2i $ x == 0
executeInstruction (IJMP k) = modify @MachineState (& #pc +~ k) >> pure mempty
executeInstruction (IJMPZ k) = do
  x <- popStack
  when (x == 0) $ modify @MachineState (& #pc +~ k)
executeInstruction IPRINT = do
  x <- popStack
  rWrite $ (<> "\n") $ T.pack $ show x
executeInstruction IREAD = do
  x <- readMaybe . T.unpack <$> rRead >>=
    maybeThrow MEInputOutput
  pushStack x

runProgram :: Machine sig m => m Sem
runProgram = do
  st <- get @MachineState
  if st ^. #pc < IM.size (st ^. #code)
     then do
       res <- executeInstruction $ (st ^. #code) IM.! (st ^. #pc)
       modify @MachineState (& #pc +~ 1)
       (res <>) <$> runProgram
     else pure ()
