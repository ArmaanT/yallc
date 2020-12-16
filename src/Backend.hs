module Backend where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    modify,
    runState,
  )
import Data.Map (Map)
import qualified Data.Map as Map
import LC4
  ( Asm (Data, Text),
    Condition (..),
    Elem (Elem),
    Imm (..),
    Instruction
      ( ADD,
        ADDI,
        AND,
        BR,
        CMP,
        CMPI,
        CONST,
        JMP,
        JSR,
        LDR,
        LEA,
        MUL,
        OR,
        RET,
        SLL,
        SRA,
        SRL,
        STR,
        SUB,
        XOR
      ),
    Lbl,
    Program (..),
    Register (R0, R1, R2, R5, R6, R7),
  )
import LL
  ( Block (isns, term),
    Bop (..),
    Cfg,
    Cnd (..),
    Fdecl (fCfg, fParam),
    Gdecl,
    Gid,
    Ginit (..),
    Insn (..),
    Lbl,
    Operand (..),
    Prog (fdecls, gdecls, tdecls),
    Tdecl,
    Terminator (..),
    Tid,
    Ty (Array, I1, I64, Namedt, Ptr, Void),
    Uid,
  )

-- | Map from Uid to offset from R5 (frame pointer)
type Layout = Map Uid Imm

-- | Map of Label to list of instructions
type LblBlocks = Map LC4.Lbl [Instruction]

-- | Global context to use
data Ctxt = Ctxt
  { types :: Map Tid Ty,
    layout :: Layout,
    funcName :: String,
    currBlock :: String,
    blocks :: LblBlocks,
    counter :: Int
  }

type Backend a = State Ctxt a

-- | Get the size of a type
sizeOf :: Ty -> Backend Int
sizeOf I64 = return 1
sizeOf (Ptr _) = return 1
sizeOf I1 = return 1
sizeOf (Array i ty) = do
  size <- sizeOf ty
  return $ i * size
sizeOf (Namedt tid) = do
  c <- get
  let ty = types c Map.! tid
  sizeOf ty
sizeOf _ = return 0

-- | Break a type to get subtype
breakTy :: Ty -> Backend Ty
breakTy (Array _ t) = return t
breakTy (Namedt tid) = do
  c <- get
  let ty = types c Map.! tid
  breakTy ty
breakTy t = return t

-- | Given a uid, return an LC4 operand that points to the uid in the stack.
uidLookup :: Uid -> Backend LC4.Imm
uidLookup u = do
  c <- get
  return $ (Map.!) (layout c) u

-- | Given an LL label, get a local LC4 label
getLocalLabel :: LL.Lbl -> Backend LC4.Lbl
getLocalLabel l = do
  c <- get
  return $ funcName c ++ "_" ++ l

-- | Compile an LL condition
compileCondition :: LL.Cnd -> LC4.Condition
compileCondition Eq = Z
compileCondition Ne = NP
compileCondition Slt = N
compileCondition Sle = NZ
compileCondition Sgt = P
compileCondition Sge = ZP

-- Compile an LL Bop into an LC4 Opcode
compileRRRBop :: Bop -> Register -> Register -> Register -> Instruction
compileRRRBop b = case b of
  Add -> ADD
  Sub -> SUB
  Mul -> MUL
  And -> AND
  Or -> OR
  Xor -> XOR
  _ -> error "not possible"

-- | Given a destination register and source LL operand return an
-- LC4 instruction that copies the operand into the register.
compileOperand :: LC4.Register -> LL.Operand -> Backend Instruction
compileOperand dest Null = return $ CONST dest (Imm 0)
compileOperand dest (Const i) = return $ CONST dest (Imm i)
compileOperand dest (Id u) = do
  imm <- uidLookup u
  return $ LDR dest R5 imm
compileOperand dest (Gid u) = return $ LEA dest u

-- | Compile a Ginit
compileGInit :: Ginit -> [Imm]
compileGInit GNull = [Imm 0]
compileGInit (GInt i) = [Imm i]
compileGInit (GArray l) = concatMap (compileGInit . snd) l

-- | Compile a Gdecl
compileGDecl :: (Gid, Gdecl) -> Elem
compileGDecl (gid, (_, ginit)) = Elem gid (Data $ compileGInit ginit)

-- | Compile a Program
compileProg :: LL.Prog -> LC4.Program
compileProg p =
  let funcs = concatMap (compileFDecl (tdecls p)) (fdecls p)
      globals = map compileGDecl (LL.gdecls p)
   in Program (globals ++ funcs)

-- | Define the local stack layout
localStackLayout :: State Int Layout -> [(Uid, Insn)] -> State Int Layout
localStackLayout s [] = s
localStackLayout s ((x, _) : xs) =
  let s' = do
        a <- s
        imm <- get
        put (imm -1)
        return $ Map.insert x (Imm imm) a
   in localStackLayout s' xs

-- | Define the argument layout
argLayout :: Int -> [Uid] -> Layout
argLayout _ [] = Map.empty
argLayout i (x : xs) = Map.insert x (Imm i) $ argLayout (i + 1) xs

-- | Define the stack layout of a function
stackLayout :: [Uid] -> Cfg -> State Int Layout
stackLayout args cfg =
  let argl = argLayout 3 args
      blkl = localStackLayout (return argl) (isns $ fst cfg)
      lblBlkl = localStackLayout blkl (concatMap (isns . snd) (snd cfg))
   in lblBlkl

-- | Given a list of instructions, append them to the current block
addInstructions :: [LC4.Instruction] -> Backend ()
addInstructions l = do
  ctxt <- get
  let currentBlock = currBlock ctxt
  let m = blocks ctxt
  modify (\x -> x {blocks = Map.insertWith (flip (++)) currentBlock l m})

-- Compile an FDecl
compileFDecl :: [Tdecl] -> (Gid, Fdecl) -> [LC4.Elem]
compileFDecl tdecls (name, fdec) =
  let cfg = fCfg fdec
      (layout, stackSpace) = runState (stackLayout (fParam fdec) cfg) (-1)
      types = Map.fromList tdecls
      context = Ctxt types layout name name Map.empty 0
   in evalState (compileFDeclAux cfg stackSpace) context
  where
    compileFDeclAux :: Cfg -> Int -> Backend [Elem]
    compileFDeclAux cfg stackSpace = do
      addInstructions $ prologue stackSpace
      compileBlock (fst cfg)
      mapM_ compileLabeledBlock (snd cfg)
      c <- get
      let blks = blocks c
      return $ Map.foldMapWithKey (\u i -> [Elem u $ Text i]) blks
    prologue size =
      [ ADDI R6 R6 (Imm (-3)), -- Add 3 spots to stack for (FP, RA, RV)
        STR R7 R6 (Imm 1), -- Save RA
        STR R5 R6 (Imm 0), -- Save FP
        ADDI R5 R6 (Imm 0), -- Set local FP
        ADDI R6 R6 (Imm $ size + 1) -- Allocate stack space
      ]

-- | Compile a block
compileBlock :: Block -> Backend ()
compileBlock b = do
  mapM_ compileInstruction (isns b)
  compileTerminator (snd $ term b)

-- | Compile a labeled block
compileLabeledBlock :: (String, Block) -> Backend ()
compileLabeledBlock (l, b) = do
  lbl <- getLocalLabel l
  modify (\x -> x {currBlock = lbl})
  compileBlock b

-- | Compile an instruction
compileInstruction :: (Uid, Insn) -> Backend ()
compileInstruction (u, Binop b _ o1 o2) = compileBopInstruction u b o1 o2
compileInstruction (u, Alloca t) = do
  imm <- uidLookup u
  addInstructions
    [ ADDI R6 R6 (Imm (-1)),
      ADDI R0 R6 (Imm 0),
      STR R0 R5 imm
    ]
compileInstruction (u, Load (Ptr t) o) = do
  op <- compileOperand R0 o
  imm <- uidLookup u
  addInstructions
    [ op,
      LDR R0 R0 (Imm 0),
      STR R0 R5 imm
    ]
compileInstruction (_, Load _ _) = error "bad load"
compileInstruction (u, Store t o1 o2) = do
  op1 <- compileOperand R0 o1
  op2 <- compileOperand R1 o2
  addInstructions
    [ op1,
      op2,
      STR R0 R1 $ Imm 0
    ]
compileInstruction (u, Icmp cnd t o1 o2) = compileICmpInstruction u cnd o1 o2
compileInstruction (u, Call t (Gid lbl) l) = compileCallInstruction u lbl l
compileInstruction (_, Call {}) = error "bad call"
compileInstruction (u, Bitcast t1 o t2) = do
  op <- compileOperand R0 o
  imm <- uidLookup u
  addInstructions [op, STR R0 R5 imm]
compileInstruction (u, Gep t o l) = compileGEPInstruction u (t, o) l

-- | Compile a Bop Instruction
compileBopInstruction :: LL.Uid -> Bop -> LL.Operand -> LL.Operand -> Backend ()
compileBopInstruction u Shl o1 o2 = compileShiftInstruction u SLL o1 o2
compileBopInstruction u Lshr o1 o2 = compileShiftInstruction u SRL o1 o2
compileBopInstruction u Ashr o1 o2 = compileShiftInstruction u SRA o1 o2
compileBopInstruction u b o1 o2 = do
  op1 <- compileOperand R0 o1
  op2 <- compileOperand R1 o2
  imm <- uidLookup u
  addInstructions
    [ op1,
      op2,
      compileRRRBop b R0 R0 R1,
      STR R0 R5 imm
    ]

-- | Compile a shift instruction
compileShiftInstruction :: LL.Uid -> (Register -> Register -> Imm -> Instruction) -> Operand -> Operand -> Backend ()
compileShiftInstruction u op o1 o2 = do
  op1 <- compileOperand R0 o1
  op2 <- compileOperand R1 o2
  imm <- uidLookup u
  c <- get
  let curr = counter c
  modify (\x -> x {counter = curr + 1})
  loopName <- getLocalLabel $ "shift_loop" ++ show curr
  endName <- getLocalLabel $ "shift_end" ++ show curr
  addInstructions
    [ op1,
      op2,
      JMP loopName
    ]
  modify (\x -> x {currBlock = loopName})
  addInstructions
    [ CMPI R1 (Imm 0),
      BR NZ endName,
      op R0 R0 (Imm 1),
      ADDI R1 R1 (Imm (-1)),
      JMP loopName
    ]
  modify (\x -> x {currBlock = endName})
  addInstructions [STR R0 R5 imm]

-- | Push Args onto the stack
pushArgs :: (LL.Ty, LL.Operand) -> Backend ()
pushArgs (_, o) = do
  op <- compileOperand R0 o
  addInstructions
    [ op, -- Load variable into R)
      STR R0 R6 (Imm (-1)), -- Store variable in stack
      ADDI R6 R6 (Imm (-1)) -- Push Stack pointer
    ]

-- | Compile a call Instruction
compileCallInstruction :: LL.Uid -> LL.Gid -> [(LL.Ty, LL.Operand)] -> Backend ()
compileCallInstruction u g l = do
  op <- uidLookup u
  mapM_ pushArgs (reverse l)
  addInstructions
    [ JSR g,
      LDR R7 R6 (Imm (-1)),
      STR R7 R5 op,
      ADDI R6 R6 (Imm (length l))
    ]

-- | Compile a GEP instruction
compileGEPInstruction :: LL.Uid -> (LL.Ty, LL.Operand) -> [LL.Operand] -> Backend ()
compileGEPInstruction u (t, o) l = do
  op <- compileOperand R0 o
  imm <- uidLookup u
  let loop :: Ty -> [Operand] -> Backend ()
      loop _ [] = return ()
      loop ty (x : xs) = do
        op1 <- compileOperand R1 x
        size <- sizeOf ty
        ty' <- breakTy ty
        addInstructions [op1, CONST R2 (Imm size), MUL R1 R1 R2, ADD R0 R0 R1]
        loop ty' xs
  addInstructions [op]
  loop t l
  addInstructions [STR R0 R5 imm]

-- | Compile an icmp instruction
compileICmpInstruction :: LL.Uid -> LL.Cnd -> LL.Operand -> LL.Operand -> Backend ()
compileICmpInstruction u cnd o1 o2 = do
  op1 <- compileOperand R0 o1
  op2 <- compileOperand R1 o2
  imm <- uidLookup u
  c <- get
  let curr = counter c
  modify (\x -> x {counter = curr + 1})
  succName <- getLocalLabel $ "icmp_succ" ++ show curr
  endName <- getLocalLabel $ "icmp_end" ++ show curr
  addInstructions
    [ op1,
      op2,
      CMP R0 R1,
      BR (compileCondition cnd) succName,
      CONST R0 (Imm 0),
      BR NZP endName
    ]
  modify (\x -> x {currBlock = succName})
  addInstructions [CONST R0 (Imm 1), BR NZP endName]
  modify (\x -> x {currBlock = endName})
  addInstructions [STR R0 R5 imm]

-- Instructions that should be run when a function terminates
functionEpilogue :: [LC4.Instruction]
functionEpilogue =
  [ ADDI R6 R5 (Imm 0), -- Set SP to be FP (Pop local stack space)
    LDR R5 R6 (Imm 0), -- Load previous FP
    LDR R7 R6 (Imm 1), -- Load previous RA
    ADDI R6 R6 (Imm 3), -- Free space used by FP, RA, RV
    RET
  ]

-- | Compile a terminator
compileTerminator :: LL.Terminator -> Backend ()
compileTerminator (Ret Void _) = addInstructions functionEpilogue
compileTerminator (Ret rt (Just o)) = do
  op <- compileOperand R0 o
  addInstructions $
    [ op,
      STR R0 R5 (Imm 2)
    ]
      ++ functionEpilogue
compileTerminator (Ret _ Nothing) = addInstructions functionEpilogue
compileTerminator (Br lbl) = do
  l <- getLocalLabel lbl
  addInstructions [JMP l]
compileTerminator (Cbr o l1 l2) = do
  op <- compileOperand R0 o
  l1' <- getLocalLabel l1
  l2' <- getLocalLabel l2
  addInstructions
    [ op,
      CMPI R0 (Imm 1),
      BR Z l1',
      JMP l2'
    ]
