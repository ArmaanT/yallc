module LC4.Ast where

type Lbl = String

-- | Available registers
data Register
  = R0
  | R1
  | R2
  | R3
  | R4
  | R5 -- Frame Pointer
  | R6 -- Stack Pointer
  | R7 -- Used for JSR/RET
  deriving (Eq, Ord, Show)

-- Immediates
newtype Imm = Imm Int deriving (Show, Eq)

-- | Conditions for BR commands
data Condition
  = P -- Gt
  | Z -- Eq
  | ZP -- Ge
  | N -- Lt
  | NP -- Neq
  | NZ -- Le
  | NZP -- Anything
  deriving (Eq, Show)

-- | Instructions
data Instruction
  = NOP
  | BR Condition Lbl
  | ADD Register Register Register
  | MUL Register Register Register
  | SUB Register Register Register
  | DIV Register Register Register
  | ADDI Register Register Imm
  | CMP Register Register
  | CMPU Register Register
  | CMPI Register Imm
  | CMPIU Register Imm
  | JSRR Register
  | JSR Lbl
  | AND Register Register Register
  | NOT Register Register
  | OR Register Register Register
  | XOR Register Register Register
  | ANDI Register Register Imm
  | LDR Register Register Imm
  | STR Register Register Imm
  | RTI
  | CONST Register Imm
  | SLL Register Register Imm
  | SRA Register Register Imm
  | SRL Register Register Imm
  | MOD Register Register Register
  | JMPR Register
  | JMP Lbl
  | HICONST Register Imm
  | TRAP Imm
  | RET
  | LC Register Lbl
  | LEA Register Lbl
  deriving (Eq, Show)

-- | LC4 ASM directives
-- data Directive
--   = Code
--   | Data
--   | ADDR Int
--   | FAlign
--   | Fill Int
--   | Stringz String
--   | Blwk Int
--   | Const String Int
--   | UConst String Int
data Asm
  = Text [Instruction]
  | Data [Imm]
  deriving (Eq, Show)

data Elem = Elem {lbl :: Lbl, asm :: Asm} deriving (Show, Eq)

-- | ASM Program type
newtype Program = Program [Elem] deriving (Eq, Show)
