module Compile where

import           Control.Carrier.Reader
import           Control.Carrier.Throw.Either
import qualified Data.IntMap as IM
import           Data.Text (Text)
import           Data.Word (Word16)

import Machine
import Syntax
import Zoo

type Compiler sig m =
  ( Has (Reader [Text]) sig m
  , Has (Throw LangError) sig m )

idx :: Compiler sig m => Word16 -> Text -> m Word16
idx k vn = do
  ctx <- ask @[Text]
  case ctx of
    []     -> throwError $ LECompile $ locate Nothing $ "unknown variable " <> vn
    (v:vs) -> if vn == v then pure k else local (const vs) $ idx (k - 1) vn

variableLocator :: Compiler sig m => Text -> m Word16
variableLocator varName = do
  l <- asks @[Text] $ fromIntegral . length
  idx (l - 1) varName

compileAE :: Compiler sig m => ArExp -> m [Instruction]
compileAE (AEVariable v) = do
  k <- variableLocator v
  pure [IGET k]
compileAE (AENumeral n) = pure [IPUSH (fromIntegral n)]
compileAE (AEPlus a b) = do
  x <- compileAE a
  y <- compileAE b
  pure $ x <> y <> [IADD]
compileAE (AEMinus a b) = do
  x <- compileAE a
  y <- compileAE b
  pure $ x <> y <> [ISUB]
compileAE (AETimes a b) = do
  x <- compileAE a
  y <- compileAE b
  pure $ x <> y <> [IMUL]
compileAE (AEDivide a b) = do
  x <- compileAE a
  y <- compileAE b
  pure $ x <> y <> [IDIV]
compileAE (AERemainder a b) = do
  x <- compileAE a
  y <- compileAE b
  pure $ x <> y <> [IMOD]

compileBE :: Compiler sig m => BoolExp -> m [Instruction]
compileBE BETrue = pure [IPUSH 1]
compileBE BEFalse = pure [IPUSH 0]
compileBE (BEEqual a b) = do
  x <- compileAE a
  y <- compileAE b
  pure $ x <> y <> [IEQ]
compileBE (BELess a b) = do
  x <- compileAE a
  y <- compileAE b
  pure $ x <> y <> [ILT]
compileBE (BEAnd a b) = do
  x <- compileBE a
  y <- compileBE b
  pure $ x <> y <> [IAND]
compileBE (BEOr a b) = do
  x <- compileBE a
  y <- compileBE b
  pure $ x <> y <> [IOR]
compileBE (BENot a) = do
  x <- compileBE a
  pure $ x <> [INOT]

compileC :: Compiler sig m => Cmd -> m [Instruction]
compileC (CNew v a w) = do
  x <- compileAE a
  local (v:) do
    y <- compileC w
    k <- variableLocator v
    pure $ x <> [ISET k] <> y
compileC CSkip = pure [INOP]
compileC (CPrint a) = do
  x <- compileAE a
  pure $ x <> [IPRINT]
compileC (CRead v) = do
  k <- variableLocator v
  pure [IREAD, ISET k]
compileC (CAssign v a) = do
  x <- compileAE a
  k <- variableLocator v
  pure $ x <> [ISET k]
compileC (CSeq w z) = do
  x <- compileC w
  y <- compileC z
  pure $ x <> y
compileC (CCond a w z) = do
  x <- compileC w
  y <- compileC z
  t <- compileBE a
  pure $ t
    <> [IJMPZ (fromIntegral $ length x + 1)]
    <> x
    <> [IJMP (fromIntegral $ length y)]
    <> y
compileC (CWhile a w) = do
  x <- compileC w
  y <- compileBE a
  let n = length x
  pure $ y
    <> [IJMPZ (fromIntegral $ n + 1)]
    <> x
    <> [IJMP (-(fromIntegral $ length y + 2 + n))]

compile :: Has (Throw LangError) sig m => Cmd -> m Program
compile c = do
  is <- runReader @[Text] [] $ compileC c
  pure $ MkProgram $ IM.fromList $ zip [0..] is
