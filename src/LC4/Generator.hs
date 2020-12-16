{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module LC4.Generator where

import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
  )
import qualified Data.Map as Map
import LC4.Ast
  ( Asm (Data, Text),
    Condition (N, NP, NZ, NZP, P, Z, ZP),
    Elem (Elem),
    Imm (Imm),
    Instruction (..),
    Lbl,
    Program (Program),
    Register (R0, R1, R2, R3, R4, R5, R6, R7),
    lbl,
  )
import QuickCheck.GenT
  ( Arbitrary (..),
    GenT,
    MonadGen (..),
    elements,
    frequency,
    getSize,
    oneof,
  )
import Utils ()

type Registers = Map.Map Register Int

type Memory = Map.Map Int Int

type Labels = [Lbl]

type Store = (Memory, Labels)

-- Load in memory address using CONST command
sequenceLdrIns :: Instruction -> Imm -> [Instruction]
sequenceLdrIns (LDR rd rs _) location =
  let constRs = CONST rs location
      ldrIns = LDR rd rs (Imm 0)
   in [constRs, ldrIns]
sequenceLdrIns ins _ = [ins]

-- Load in value to store using CONST command
sequenceStrIns :: Instruction -> Imm -> Imm -> [Instruction]
sequenceStrIns (STR rt rs offset) location value =
  let constRt = CONST rt value
      constRs = CONST rs location
      strIns = STR rt rs offset
   in [constRt, constRs, strIns]
sequenceStrIns ins _ _ = [ins]

genCondition :: (MonadGen m, MonadState Store m) => m Condition
genCondition = elements [P, Z, ZP, N, NP, NZ, NZP]

genRegister :: (MonadGen m, MonadState Store m) => m Register
genRegister = elements [R0, R1, R2, R3, R4, R5, R6, R7]

genImm :: (MonadGen m, MonadState Store m) => m Imm
genImm = Imm <$> choose (-10, 10)

genUnaryIns :: (MonadGen m, MonadState Store m) => (Register -> Register -> Instruction) -> m Instruction
genUnaryIns f =
  f <$> genRegister <*> genRegister

genUnaryImmIns :: (MonadGen m, MonadState Store m) => (Register -> Imm -> Instruction) -> m Instruction
genUnaryImmIns f =
  f <$> genRegister <*> genImm

genBinaryIns :: (MonadGen m, MonadState Store m) => (Register -> Register -> Register -> Instruction) -> m Instruction
genBinaryIns f =
  f <$> genRegister <*> genRegister <*> genRegister

genBinaryImmIns :: (MonadGen m, MonadState Store m) => (Register -> Register -> Imm -> Instruction) -> m Instruction
genBinaryImmIns f =
  f <$> genRegister <*> genRegister <*> genImm

genBitShiftIns :: (MonadGen m, MonadState Store m) => (Register -> Register -> Imm -> Instruction) -> m Instruction
genBitShiftIns f =
  f <$> genRegister <*> genRegister <*> genImm

insGenerators :: (MonadGen m, MonadState Store m) => m Instruction
insGenerators =
  let binaryOps =
        [ genBinaryIns ADD,
          genBinaryIns SUB,
          genBinaryIns MUL,
          genBinaryIns DIV,
          genBinaryIns AND,
          genBinaryIns OR,
          genBinaryIns XOR,
          genBinaryIns MOD
        ]
      binaryImmOps =
        [ genBinaryImmIns ADDI,
          genBinaryImmIns ANDI,
          genBinaryImmIns LDR,
          genBinaryImmIns STR
        ]
      bitShiftOps =
        [ genBitShiftIns SLL,
          genBitShiftIns SRA,
          genBitShiftIns SRL
        ]
      unaryOps =
        [ genUnaryIns CMP,
          genUnaryIns CMPU,
          genUnaryIns NOT
        ]
      unaryImmOps =
        [ genUnaryImmIns CMPI,
          genUnaryImmIns CMPIU,
          genUnaryImmIns CONST,
          genUnaryImmIns HICONST
        ]
   in oneof $ binaryOps ++ binaryImmOps ++ bitShiftOps ++ unaryOps ++ unaryImmOps

genInstruction :: (MonadGen m, MonadState Store m) => m [Instruction]
genInstruction = do
  (mem, lbls) <- get
  ins <- insGenerators
  case ins of
    STR _ _ (Imm imm6) -> do
      location <- elements [0x2000 .. 0x7FFF]
      Imm insert <- genImm
      put (Map.insert (location + imm6) insert mem, lbls)
      return $ sequenceStrIns ins (Imm location) (Imm insert)
    LDR {} -> do
      put (mem, lbls)
      if Map.size mem > 0
        then sequenceLdrIns ins <$> fmap Imm (elements (Map.keys mem))
        else return []
    _ -> do
      put (mem, lbls)
      return [ins]

genForLoop :: (MonadGen m, MonadState Store m) => m [Elem]
genForLoop = do
  (mem, lbls) <- get
  i <- Imm <$> choose (1, 10)
  loopName <- genLbl
  address <- Imm <$> elements [0x2000 .. 0x7FFF]
  let prologue =
        Elem (loopName ++ "_begin") $
          Text
            [ CONST R0 i,
              CONST R1 address,
              STR R0 R1 (Imm 0)
            ]
  body <- Elem (loopName ++ "_body") <$> genBlock
  let epilogue =
        Elem (loopName ++ "_end") $
          Text
            [ CONST R1 address,
              LDR R0 R1 (Imm 0),
              ADDI R0 R0 (Imm (-1)),
              STR R0 R1 (Imm 0),
              CMPI R0 (Imm 0),
              BR ZP (loopName ++ "_body")
            ]
  put (mem, lbls)
  return [prologue, body, epilogue]

genIfStatement :: (MonadGen m, MonadState Store m) => m [Elem]
genIfStatement = do
  (mem, lbls) <- get
  cmp <- oneof [genUnaryIns CMP, genUnaryImmIns CMPI, genUnaryImmIns CMPIU]
  condition <- genCondition
  loopName <- genLbl
  let prologue = Elem (loopName ++ "_begin") $ Text [cmp, BR condition (loopName ++ "_end")]
  body <- Elem (loopName ++ "_body") <$> genBlock
  let epilogue = Elem (loopName ++ "_end") $ Text []
  put (mem, lbls)
  return [prologue, body, epilogue]

genIfElseStatement :: (MonadGen m, MonadState Store m) => m [Elem]
genIfElseStatement = do
  (mem, lbls) <- get
  cmp <- oneof [genUnaryIns CMP, genUnaryImmIns CMPI, genUnaryImmIns CMPIU]
  condition <- genCondition
  loopName <- genLbl
  let prologue = Elem (loopName ++ "_begin") $ Text [cmp, BR condition (loopName ++ "_else")]
  ifBody <-
    Elem (loopName ++ "_body")
      <$> (appendInstructions <$> genBlock <*> return [BR NZP (loopName ++ "_end")])
  put (mem, lbls)
  elseBody <- Elem (loopName ++ "_else") <$> genBlock
  let epilogue = Elem (loopName ++ "_end") $ Text []
  put (mem, lbls)
  return [prologue, ifBody, elseBody, epilogue]

appendInstructions :: Asm -> [Instruction] -> Asm
appendInstructions (Text asm) ins = Text $ asm ++ ins
appendInstructions asm@(Data _) _ = asm

genLbl :: (MonadGen m, MonadState Store m) => m Lbl
genLbl =
  let pool = ['a' .. 'z'] ++ ['1' .. '9']
   in frequency [(1, (: []) <$> elements pool), (8, (:) <$> elements pool <*> genLbl)]

genBlock :: (MonadGen m, MonadState Store m) => m Asm
genBlock =
  frequency
    [ (1, Text <$> genInstruction),
      (8, appendInstructions <$> genBlock <*> genInstruction)
    ]

genElem :: (MonadGen m, MonadState Store m) => m Elem
genElem = do
  (mem, lbl) <- get
  label <- genLbl
  put (mem, label : lbl)
  Elem label <$> genBlock

genElems :: (MonadGen m, MonadState Store m) => m [Elem]
genElems =
  frequency
    [ (1, (: []) <$> genElem),
      (8, (:) <$> genElem <*> genElems),
      (2, (++) <$> genForLoop <*> genElems),
      (2, (++) <$> genIfStatement <*> genElems),
      (2, (++) <$> genIfElseStatement <*> genElems)
    ]

genProgram :: (MonadGen m, MonadState Store m) => m Program
genProgram = do
  elems <- genElems
  case elems of
    [] -> return $ Program [Elem "main" (Text [RET])]
    (e : _) ->
      let end_jump = Elem "end_jump" (Text [JSR "main_epilouge"])
          main_prologue = Elem "main" (Text [JSR (lbl e)])
          main_epilogue = Elem "main_epilouge" (Text [STR R0 R6 (Imm (-1)), RET])
       in return $ Program $ elems ++ [end_jump, main_prologue, main_epilogue]

instance Arbitrary Program where
  arbitrary =
    let initState = (Map.empty, [])
        gen = runStateT genProgram initState
        gen' = fst <$> gen
     in gen'
