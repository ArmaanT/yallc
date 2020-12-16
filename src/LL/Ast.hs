module LL.Ast where

-- | Local IDs
type Uid = String

-- | Global IDs
type Gid = String

-- | Named Types
type Tid = String

-- | Labels
type Lbl = String

-- | Types
data Ty
  = Void
  | I1
  | I8
  | I64
  | Ptr Ty
  | Array Int Ty
  | Fun [Ty] Ty
  | Namedt Tid
  deriving (Show, Eq)

-- | Function Type
type FTy = ([Ty], Ty)

-- | Operands
data Operand
  = Null
  | Const Int
  | Gid Gid
  | Id Uid
  deriving (Show, Eq)

-- | Binary Operands
data Bop
  = Add
  | Sub
  | Mul
  | Shl
  | Lshr
  | Ashr
  | And
  | Or
  | Xor
  deriving (Show, Eq)

-- | Comparision operators
data Cnd
  = Eq
  | Ne
  | Slt
  | Sle
  | Sgt
  | Sge
  deriving (Show, Eq)

-- | Instruction
data Insn
  = Binop Bop Ty Operand Operand
  | Alloca Ty
  | Load Ty Operand
  | Store Ty Operand Operand
  | Icmp Cnd Ty Operand Operand
  | Call Ty Operand [(Ty, Operand)]
  | Bitcast Ty Operand Ty
  | Gep Ty Operand [Operand]
  deriving (Show, Eq)

-- | Terminating Insn
data Terminator
  = Ret Ty (Maybe Operand)
  | Br Lbl
  | Cbr Operand Lbl Lbl
  deriving (Show, Eq)

-- | Block
data Block = Block {isns :: [(Uid, Insn)], term :: (Uid, Terminator)} deriving (Show, Eq)

-- | Control flow graph
type Cfg = (Block, [(Lbl, Block)])

-- | Function declaration
data Fdecl = Fdecl {fTy :: FTy, fParam :: [Uid], fCfg :: Cfg} deriving (Show, Eq)

-- | Global Data
data Ginit
  = GNull
  | GInt Int
  | GArray [(Ty, Ginit)]
  deriving (Show, Eq)

-- | Global declaration
type Gdecl = (Ty, Ginit)

-- | Type declaration
type Tdecl = (Tid, Ty)

-- | LLVMlite Program
data Prog = Prog {tdecls :: [Tdecl], gdecls :: [(Gid, Gdecl)], fdecls :: [(Gid, Fdecl)]} deriving (Show, Eq)
