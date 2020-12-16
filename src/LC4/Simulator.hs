{-# LANGUAGE FlexibleContexts #-}

module LC4.Simulator where

import Control.Monad.Except (runExceptT, MonadError (throwError) )
import Control.Monad.State (MonadState (get, put), runState)
import Data.Bits
  ( Bits (complement, shiftL, shiftR, xor, (.&.), (.|.)),
  )
import Data.List (isPrefixOf)
import Data.Map as Map (Map, empty, fromList, insert, lookup)
import Data.Maybe (fromMaybe)
import LC4.Ast
  ( Asm (..),
    Condition (..),
    Elem (asm, lbl),
    Imm (Imm),
    Instruction (..),
    Lbl,
    Program (..),
    Register (R0, R1, R2, R3, R4, R5, R6, R7),
  )
import Data.Int ( Int16 )

type Registers = Map Register Int

type Memory = Map Int Int

type Store = (Int, Condition, Registers, Memory, Bool, Lbl)

type Context = (Map Lbl Int, Map Int Instruction, Map Int Int, Map Int Lbl)

initReg :: Registers
initReg =
  Map.fromList
    [ (R0, 0),
      (R1, 0),
      (R2, 0),
      (R3, 0),
      (R4, 0),
      (R5, 0x7FFF),
      (R6, 0x7FFF),
      (R7, 0)
    ]

initMem :: Map Int Int -> Memory
initMem = Map.insert 0x7FFF 0

simulate :: Program -> Either String Int
simulate program =
  let lbls = labelMap program 0 Map.empty
      insMap = instructionMap program 0 Map.empty
      dMap = dataMap program 0 Map.empty
      reverseLbls = reverseLabelMap program 0 Map.empty
      (startIns, pc) = startInstruction insMap lbls
      initState = (pc, Z, initReg, initMem dMap, False, "main")
      context = (lbls, insMap, dMap, reverseLbls)
      (result, _) = runState (runExceptT (evalProgram startIns context)) initState
   in result

evalProgram :: (MonadError String m, MonadState Store m) => Instruction -> Context -> m Int
evalProgram ins context = do
  execProgram ins context
  (_, _, reg, mem, _, _) <- get
  spValue <- dereference reg R6
  dereference mem (spValue - 1)

execProgram :: (MonadError String m, MonadState Store m) => Instruction -> Context -> m ()
execProgram ins context = do
  (_, _, _, _, _, lbl) <- get
  nextIns <- execStep ins context
  if nextIns == NOP || (isPrefixOf "main" lbl && nextIns == RET)
    then return ()
    else execProgram nextIns context

execStep :: (MonadError String m, MonadState Store m) => Instruction -> Context -> m Instruction
execStep ins context@(_, insMap, _, reverseLbls) = do
  execInstruction ins context
  (pc, nzp, reg, mem, psr, _) <- get
  lbl <- dereference reverseLbls pc
  put (pc, nzp, reg, mem, psr, lbl)
  if pc == 0x80FF
    then return NOP
    else do dereference insMap pc

integerToCondition :: Int -> Condition
integerToCondition n
  | n < 0 = N
  | n == 0 = Z
  | otherwise = P

compareCondition :: Condition -> Condition -> Bool
compareCondition NZP _ = True
compareCondition NZ c = (c == N) || (c == Z)
compareCondition NP c = (c == N) || (c == P)
compareCondition ZP c = (c == Z) || (c == P)
compareCondition N c = (==) N c
compareCondition Z c = (==) Z c
compareCondition P c = (==) P c

asmCountLines :: Asm -> Int -> Int
asmCountLines (Text []) n = n
asmCountLines (Data []) n = n
asmCountLines (Text (_ : ls)) n = asmCountLines (Text ls) (n + 1)
asmCountLines (Data (_ : ls)) n = asmCountLines (Data ls) (n + 1)

labelMap :: Program -> Int -> Map Lbl Int -> Map Lbl Int
labelMap (Program []) _ map = map
labelMap (Program (e : es)) n map =
  let label = lbl e
      count = asmCountLines (asm e) 0
   in labelMap (Program es) (n + count) (Map.insert label n map)

insertCodeBlock :: [Instruction] -> Int -> Map Int Instruction -> (Map Int Instruction, Int)
insertCodeBlock [] n map = (map, n)
insertCodeBlock (i : is) n map =
  insertCodeBlock is (n + 1) (Map.insert n i map)

instructionMap :: Program -> Int -> Map Int Instruction -> Map Int Instruction
instructionMap (Program []) _ map = map
instructionMap (Program (e : es)) n map =
  case asm e of
    Text is ->
      let (map', n') = insertCodeBlock is n map
       in instructionMap (Program es) n' map'
    Data _ ->
      let n' = asmCountLines (asm e) n
       in instructionMap (Program es) n' map

insertDataBlock :: [Imm] -> Int -> Map Int Int -> (Map Int Int, Int)
insertDataBlock [] n map = (map, n)
insertDataBlock ((Imm d) : ds) n map =
  insertDataBlock ds (n + 1) (Map.insert n d map)

dataMap :: Program -> Int -> Map Int Int -> Map Int Int
dataMap (Program []) _ map = map
dataMap (Program (e : es)) n map =
  case asm e of
    Text _ ->
      let n' = asmCountLines (asm e) n
       in dataMap (Program es) n' map
    Data ds ->
      let (map', n') = insertDataBlock ds n map
       in dataMap (Program es) n' map'

reverseLabelMap :: Program -> Int -> Map Int Lbl -> Map Int Lbl
reverseLabelMap (Program []) _ map = map
reverseLabelMap (Program (e : es)) n map =
  case asm e of
    Text _ ->
      let n' = asmCountLines (asm e) n
          map' = foldl (\acc x -> Map.insert x (lbl e) acc) map [n .. (n' - 1)]
       in reverseLabelMap (Program es) n' map'
    Data _ ->
      let n' = asmCountLines (asm e) n
       in reverseLabelMap (Program es) n' map

startInstruction :: Map Int Instruction -> Map Lbl Int -> (Instruction, Int)
startInstruction locations labels
  | null locations || null labels = (NOP, 0)
  | otherwise = case Map.lookup "main" labels of
    Just location -> (fromMaybe NOP (Map.lookup location locations), location)
    Nothing -> (NOP, 0)

dereference :: (Ord a, Show a, Show b, MonadError String m) => Map a b -> a -> m b
dereference reg r =
  case Map.lookup r reg of
    Just x -> return x
    Nothing -> throwError $ "Invalid register lookup: key = " ++ show r ++ " map = " ++ show reg

execBinaryInstruction :: (MonadError String m, MonadState Store m) => (Int -> Int -> Int) -> Register -> Register -> Register -> m ()
execBinaryInstruction op rd rs rt = do
  (pc, _, reg, mem, psr, lbl) <- get
  rsValue <- dereference reg rs
  rtValue <- dereference reg rt
  let insert = rsValue `op` rtValue
  let nzp' = integerToCondition insert
  put (pc + 1, nzp', Map.insert rd insert reg, mem, psr, lbl)

execBinaryImmInstruction :: (MonadError String m, MonadState Store m) => (Int -> Int -> Int) -> Register -> Register -> Imm -> m ()
execBinaryImmInstruction op rd rs (Imm imm) = do
  (pc, _, reg, mem, psr, lbl) <- get
  rsValue <- dereference reg rs
  let insert = rsValue `op` imm
  let nzp' = integerToCondition insert
  put (pc + 1, nzp', Map.insert rd insert reg, mem, psr, lbl)

execInstruction :: (MonadError String m, MonadState Store m) => Instruction -> Context -> m ()
execInstruction (BR nzpCondition dest) (lbls, _, _, _) = do
  (pc, nzp, reg, mem, psr, lbl) <- get
  pc' <- dereference lbls dest
  let jump = compareCondition nzpCondition nzp
  if jump
    then put (pc', nzp, reg, mem, psr, dest)
    else put (pc + 1, nzp, reg, mem, psr, lbl)
execInstruction (ADD rd rs rt) _ = execBinaryInstruction (+) rd rs rt
execInstruction (MUL rd rs rt) _ = execBinaryInstruction (*) rd rs rt
execInstruction (SUB rd rs rt) _ = execBinaryInstruction (-) rd rs rt
execInstruction (DIV rd rs rt) _ =
  let safeDiv x y = if y == 0 then 0 else x `div` y
   in execBinaryInstruction safeDiv rd rs rt
execInstruction (ADDI rd rs imm5) _ = execBinaryImmInstruction (+) rd rs imm5
execInstruction (MOD rd rs rt) _ =
  let safeMod x y = if y == 0 then 0 else x `mod` y
   in execBinaryInstruction safeMod rd rs rt
execInstruction (AND rd rs rt) _ = execBinaryInstruction (.&.) rd rs rt
execInstruction (NOT rd rs) _ = do
  (pc, _, reg, mem, psr, lbl) <- get
  rsValue <- dereference reg rs
  let insert = complement rsValue
  let nzp' = integerToCondition insert
  put (pc + 1, nzp', Map.insert rd insert reg, mem, psr, lbl)
execInstruction (OR rd rs rt) _ = execBinaryInstruction (.|.) rd rs rt
execInstruction (XOR rd rs rt) _ = execBinaryInstruction xor rd rs rt
execInstruction (ANDI rd rs imm5) _ = execBinaryImmInstruction (.&.) rd rs imm5
execInstruction (LDR rd rs (Imm imm6)) _ = do
  (pc, _, reg, mem, psr, lbl) <- get
  rsValue <- dereference reg rs
  let address = imm6 + rsValue
  insert <- dereference mem address
  let nzp' = integerToCondition insert
  put (pc + 1, nzp', Map.insert rd insert reg, mem, psr, lbl)
execInstruction (STR rt rs (Imm imm6)) _ = do
  (pc, nzp, reg, mem, psr, lbl) <- get
  rsValue <- dereference reg rs
  let address = imm6 + rsValue
  insert <- dereference reg rt
  put (pc + 1, nzp, reg, Map.insert address insert mem, psr, lbl)
execInstruction (CONST rd (Imm imm9)) _ = do
  (pc, _, reg, mem, psr, lbl) <- get
  let nzp' = integerToCondition imm9
  put (pc + 1, nzp', Map.insert rd imm9 reg, mem, psr, lbl)
execInstruction (HICONST rd (Imm uimm8)) _ = do
  (pc, _, reg, mem, psr, lbl) <- get
  rdValue <- dereference reg rd
  let insert = rdValue .|. shiftL uimm8 8
  let insert16 = fromIntegral (insert :: Int) :: Int16
  let insert' = fromIntegral (insert16 :: Int16) :: Int
  let nzp' = integerToCondition insert'
  put (pc + 1, nzp', Map.insert rd insert' reg, mem, psr, lbl)
execInstruction (CMP rs rt) _ = do
  (pc, _, reg, mem, psr, lbl) <- get
  rsValue <- dereference reg rs
  rtValue <- dereference reg rt
  let nzp' = integerToCondition $ rsValue - rtValue
  put (pc + 1, nzp', reg, mem, psr, lbl)
execInstruction (CMPU rs rt) _ = do
  (pc, _, reg, mem, psr, lbl) <- get
  rsValue <- dereference reg rs
  rtValue <- dereference reg rt
  let rsValueUnsigned = fromIntegral rsValue :: Word
  let rtValueUnsigned = fromIntegral rtValue :: Word
  let diff = fromIntegral . toInteger $ rsValueUnsigned - rtValueUnsigned :: Int
  let nzp' = integerToCondition diff
  put (pc + 1, nzp', reg, mem, psr, lbl)
execInstruction (CMPI rs (Imm imm7)) _ = do
  (pc, _, reg, mem, psr, lbl) <- get
  rsValue <- dereference reg rs
  let nzp' = integerToCondition $ rsValue - imm7
  put (pc + 1, nzp', reg, mem, psr, lbl)
execInstruction (CMPIU rs (Imm uimm7)) _ = do
  (pc, _, reg, mem, psr, lbl) <- get
  rsValue <- dereference reg rs
  let nzp' = integerToCondition $ rsValue - uimm7
  put (pc + 1, nzp', reg, mem, psr, lbl)
execInstruction (SLL rd rs uimm4) _ =
  let safeShiftL x y = fromInteger $ shiftL (toInteger x) y
   in execBinaryImmInstruction safeShiftL rd rs uimm4
execInstruction (SRA rd rs uimm4) _ =
  let safeShiftR x y = fromInteger $ shiftL (toInteger x) y
   in execBinaryImmInstruction safeShiftR rd rs uimm4
execInstruction (SRL rd rs uimm4) _ =
  let logicalShiftR x y = fromIntegral (fromIntegral x `shiftR` y :: Word)
   in execBinaryImmInstruction logicalShiftR rd rs uimm4
execInstruction (JSRR rs) _ = do
  (pc, _, reg, mem, psr, lbl) <- get
  pc' <- dereference reg rs
  let insert = pc + 1
  let nzp' = integerToCondition insert
  put (pc', nzp', Map.insert R7 insert reg, mem, psr, lbl)
execInstruction (JSR dest) (lbls, _, _, _) = do
  (pc, _, reg, mem, psr, _) <- get
  pc' <- dereference lbls dest
  let insert = pc + 1
  let nzp' = integerToCondition insert
  put (pc', nzp', Map.insert R7 insert reg, mem, psr, dest)
execInstruction (JMPR rs) _ = do
  (_, nzp, reg, mem, psr, lbl) <- get
  pc' <- dereference reg rs
  put (pc', nzp, reg, mem, psr, lbl)
execInstruction (JMP dest) (lbls, _, _, _) = do
  (_, nzp, reg, mem, psr, _) <- get
  pc' <- dereference lbls dest
  put (pc', nzp, reg, mem, psr, dest)
execInstruction (TRAP (Imm uimm8)) _ = do
  (pc, _, reg, mem, _, lbl) <- get
  let insert = pc + 1
  let nzp' = integerToCondition insert
  let pc' = 0x8000 .|. uimm8
  put (pc', nzp', Map.insert R7 insert reg, mem, True, lbl)
execInstruction RTI _ = do
  (pc, nzp, reg, mem, _, lbl) <- get
  pc' <- dereference reg R7
  put (pc', nzp, reg, mem, False, lbl)
execInstruction RET _ = do
  (pc, nzp, reg, mem, psr, lbl) <- get
  pc' <- dereference reg R7
  put (pc', nzp, reg, mem, psr, lbl)
execInstruction (LEA rd dest) (lbls, _, _, _) = do
  (pc, _, reg, mem, psr, lbl) <- get
  insert <- dereference lbls dest
  let nzp' = integerToCondition insert
  put (pc + 1, nzp', Map.insert rd insert reg, mem, psr, lbl)
  return ()
execInstruction (LC rd dest) (lbls, _, dMap, _) = do
  (pc, _, reg, mem, psr, lbl) <- get
  ref <- dereference lbls dest
  insert <- dereference mem ref
  let nzp' = integerToCondition insert
  put (pc + 1, nzp', Map.insert rd insert reg, mem, psr, lbl)
  return ()
execInstruction NOP _ = do
  (pc, nzp, reg, mem, psr, lbl) <- get
  put (pc + 1, nzp, reg, mem, psr, lbl)
