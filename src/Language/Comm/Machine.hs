{-|
  A simple machine with a program, RAM, program counter and a stack pointer. The program
  is an array of instructions. The stack grows downwards. Arithmetical and boolean
  operations operate on the stack.
-}
module Language.Comm.Machine where

import Control.Carrier.State.Strict
import Control.Carrier.Throw.Either
import Control.Effect.Runtime
import Control.Exception (Exception)
import Control.Lens
import Control.Monad (when)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Formatting
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Zoo

-- | The machine instructions.
data Instruction
  = INOP -- ^ no operation
  | ISET !Word16 -- ^ pop from stack and store in the given location
  | IGET !Word16 -- ^ push from given location onto stack
  | IPUSH !Word16 -- ^ push integer constant onto stack
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
  | IJMP !Word16 -- ^ relative jump
  | IJMPZ !Word16 -- ^ relative jump if zero on the stack
  | IPRINT -- ^ pop from stack and print
  | IREAD -- ^ read and push on stack
  deriving (Eq, Generic)

shInst :: Text -> Word16 -> String
shInst = formatToString ((right 6 ' ' %. stext) % " " % "0x" %(left 4 '0' %. hex))

instance Show Instruction where
  show INOP      = "NOP"
  show (ISET x)  = shInst "SET" x
  show (IGET x)  = shInst "GET" x
  show (IPUSH x) = shInst "PUSH" x
  show IADD      = "ADD"
  show ISUB      = "SUB"
  show IMUL      = "MUL"
  show IDIV      = "DIV"
  show IMOD      = "MOD"
  show IEQ       = "EQ"
  show ILT       = "LT"
  show IAND      = "AND"
  show IOR       = "OR"
  show INOT      = "NOT"
  show (IJMP x)  = shInst "JMP" x
  show (IJMPZ x) = shInst "JMPZ" x
  show IPRINT    = "PRINT"
  show IREAD     = "READ"

newtype Program = MkProgram { unProgram :: IntMap Instruction }

instance Show Program where
  show (MkProgram p) = IM.foldlWithKey' (\s k i -> s <> formatToString
    ( "0x" % (left 4 '0' %. hex) % " " % shown % "\n") k i )
    "" p

-- | Machine errors.
data MachineError
  = MEIllegalAddress
  | MEIllegalInstruction
  | MEZeroDivision
  | MEInvalidInput

instance Show MachineError where
  show MEIllegalAddress     = "illegal address"
  show MEIllegalInstruction = "illegal instruction"
  show MEZeroDivision       = "division by zero"
  show MEInvalidInput       = "invalid input"

instance Exception MachineError

-- | The state of the machine
data MachineState = MkMachineState
  { code :: !Program -- ^ program
  , ram  :: !(IntMap Word16) -- ^ memory
  , pc   :: !Word16 -- ^ program counter
  , sp   :: !Word16 } -- ^ stack pointer
  deriving (Generic, Show)

type Machine sig m =
  ( Has (State MachineState) sig m
  , Has (Throw MachineError) sig m
  , Has Runtime sig m )

mkMachineState :: Program -> Word16 -> MachineState
mkMachineState prog ramSize = MkMachineState
  { code = prog
  , ram = IM.fromList $ zip [0..] $ replicate (fromIntegral ramSize) 0
  , pc = 0
  , sp = ramSize - 1 }

readMem :: Machine sig m => Word16 -> m Word16
readMem k = gets (IM.lookup (fromIntegral k) . ram) >>= maybeThrow MEIllegalAddress

writeMem :: Machine sig m => Word16 -> Word16 -> m ()
writeMem k x = do
  st <- get @MachineState
  if k < 0 || k >= fromIntegral (IM.size (st ^. #ram))
     then throwError MEIllegalAddress
     else put $ st & #ram %~ IM.insert (fromIntegral k) x

popStack :: Machine sig m => m Word16
popStack = do
  oldSP <- gets @MachineState sp
  x <- readMem oldSP
  let newSP = oldSP + 1
  modify @MachineState (& #sp .~ newSP)
  pure x

pushStack :: Machine sig m => Word16 -> m ()
pushStack x = do
  oldSP <- gets @MachineState sp
  let newSP = oldSP - 1
  modify @MachineState (& #sp .~ newSP)
  writeMem newSP x

b2i :: Bool -> Word16
b2i False = 0
b2i True  = 1

executeInstruction :: Machine sig m => Instruction -> m ()
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
  rWrite $ (<> "\n") $ T.pack $
    if x >= 32768
       then "-" <> show (65536 - fromIntegral @Word16 @Int x)
       else show $ fromIntegral @Word16 @Int x
executeInstruction IREAD = do
  x <- readMaybe . T.unpack <$> rRead >>=
    maybeThrow MEInvalidInput
  pushStack x

runProgram :: Machine sig m => m ()
runProgram = do
  st <- get @MachineState
  if st ^. #pc < st ^. #code . to (fromIntegral . IM.size . unProgram)
     then do
       res <- executeInstruction $ (st ^. #code . to unProgram) IM.! fromIntegral (st ^. #pc)
       modify @MachineState (& #pc +~ 1)
       (res <>) <$> runProgram
     else pure ()
