module LL.Parser where

import Data.Maybe (isNothing)
import LL.Ast
  ( Block (Block),
    Bop (..),
    Cfg,
    Cnd (..),
    Fdecl (Fdecl),
    Gdecl,
    Gid,
    Ginit (..),
    Insn (..),
    Lbl,
    Operand (..),
    Prog (Prog),
    Tdecl,
    Terminator (..),
    Ty (Array, I1, I64, I8, Namedt, Ptr, Void),
    Uid,
  )
import Parser (betweenChars, commaSpaceP, idP, intP)
import Text.Parsec
  ( char,
    many,
    many1,
    newline,
    optionMaybe,
    sepBy,
    sepBy1,
    space,
    string,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)

-- | Parse a Uid
uidP :: Parser String
uidP = char '%' *> idP

-- | Parse a Gid
gidP :: Parser String
gidP = char '@' *> idP

-- | Parse a pointer type
ptrP :: Ty -> Parser Ty
ptrP ty = do
  ptr <- optionMaybe (char '*')
  if isNothing ptr then return ty else ptrP (Ptr ty)

-- | Parse a Ty
tyP :: Parser Ty
tyP = do
  ty <- nonPtr
  ptrP ty
  where
    nonPtr =
      Void <$ string "void"
        <|> char 'i' *> iP
        <|> Array <$> (char '[' *> intP) <*> (space *> string "x" *> space *> tyP) <* char ']'
        -- <|> Fun <$ string ""
        <|> Namedt <$> uidP
    iP = I1 <$ char '1' <|> I8 <$ char '8' <|> I64 <$ string "64"

-- | Parse an Operand
operandP :: Parser Operand
operandP =
  Null <$ string "null"
    <|> Const <$> intP
    <|> Gid <$> gidP
    <|> Id <$> uidP

-- | Parse a Bop
bopP :: Parser Bop
bopP =
  try (Add <$ string "add")
    <|> try (Sub <$ string "sub") --try b/c shared prefix with shl
    <|> try (Mul <$ string "mul")
    <|> try (Shl <$ string "shl")
    <|> try (Lshr <$ string "lshr")
    <|> try (Ashr <$ string "ashr") --try b/c shared prefix with and
    <|> try (And <$ string "and")
    <|> try (Or <$ string "or")
    <|> try (Xor <$ string "xor")

-- | Parse a Cnd
cndP :: Parser Cnd
cndP =
  Eq <$ string "eq"
    <|> try (Ne <$ string "ne")
    <|> try (Slt <$ string "slt") -- try b/c shared prefix
    <|> try (Sle <$ string "sle") -- try b/c shared prefix
    <|> try (Sgt <$ string "sgt") -- try b/c shared prefix
    <|> try (Sge <$ string "sge")

-- | Parse a Store Insn
storeinsnP :: Parser Insn
storeinsnP = try (Store <$> (string "store" *> space *> tyP <* space) <*> operandP <* commaSpaceP <* tyP <* space <*> operandP)

-- | Parse an Insn
insnP :: Parser Insn
insnP =
  try (Alloca <$> (string "alloca" *> space *> tyP))
    <|> try (Load <$> (string "load" *> space *> tyP *> commaSpaceP *> tyP <* space) <*> operandP)
    <|> try (Icmp <$> (string "icmp" *> space *> cndP <* space) <*> tyP <* space <*> operandP <* commaSpaceP <*> operandP)
    <|> try (Call <$> (string "call" *> space *> tyP <* space) <*> operandP <*> (char '(' *> sepBy argP commaSpaceP <* char ')'))
    <|> try (Bitcast <$> (string "bitcast" *> space *> tyP <* space) <*> operandP <* space <* string "to" <* space <*> tyP)
    <|> try (Gep <$> (string "getelementptr" *> space *> tyP <* commaSpaceP <* tyP <* space) <*> operandP <* commaSpaceP <*> sepBy1 (string "i32" *> space *> operandP) commaSpaceP)
    <|> try (Binop <$> bopP <* space <*> tyP <* space <*> operandP <* commaSpaceP <*> operandP)
  where
    argP :: Parser (Ty, Operand)
    argP = (,) <$> tyP <* space <*> operandP

-- | Parse a Terminator
terminatorP :: Parser Terminator
terminatorP =
  Ret <$> (string "ret" *> space *> tyP) <*> ((space <|> pure ' ') *> optionMaybe operandP)
    <|> string "br" *> space *> brP
  where
    brP =
      Br <$> (string "label" *> space *> uidP)
        <|> Cbr <$> (string "i1" *> space *> operandP <* commaSpaceP) <*> (string "label" *> space *> uidP <* commaSpaceP) <*> (string "label" *> space *> uidP)

-- | Parse a Block
blockP :: Parser Block
blockP = Block <$> many (try (space *> space *> lblInsnP <* newline)) <*> (space *> space *> lblTermP)
  where
    lblInsnP :: Parser (Uid, Insn)
    lblInsnP = try ((,) <$> (uidP <* space <* char '=' <* space) <*> insnP) <|> (,) "_" <$> storeinsnP
    lblTermP :: Parser (Uid, Terminator)
    lblTermP = (,) "_" <$> terminatorP

-- | Parse a Cfg
cfgP :: Parser Cfg
cfgP = (,) <$> blockP <*> many (try lblblockP)
  where
    lblblockP :: Parser (Lbl, Block)
    lblblockP = (,) <$> (newline *> idP <* char ':' <* newline) <*> blockP

-- | Parse an argument
argP :: Parser (Ty, Uid)
argP = (,) <$> tyP <* space <*> uidP

-- | Parse a function declaration
fdeclP :: Parser (Gid, Fdecl)
fdeclP = do
  string "define" <* space
  rty <- tyP <* space
  fname <- gidP
  args <- betweenChars '(' (sepBy argP commaSpaceP) ')'
  cfg <- space *> char '{' *> newline *> cfgP <* newline <* char '}'
  return (fname, Fdecl (Prelude.map fst args, rty) (Prelude.map snd args) cfg)

-- | Parse a Ginit
ginitP :: Parser Ginit
ginitP =
  GNull <$ string "null"
    <|> GInt <$> intP
    <|> GArray <$> (char '[' *> space *> sepBy1 gargP commaSpaceP <* space <* char ']')
  where
    gargP :: Parser (Ty, Ginit)
    gargP = (,) <$> tyP <* space <*> ginitP

-- | Parse a GDecl
gdeclP :: Parser Gdecl
gdeclP = (,) <$> tyP <* space <*> ginitP

-- | Parse a global
globalP :: Parser (Gid, Gdecl)
globalP = (,) <$> gidP <* space <* char '=' <* space <* string "global" <* space <*> gdeclP

-- | Parse a type
tdeclP :: Parser Tdecl
tdeclP = (,) <$> uidP <* space <* char '=' <* space <* string "type" <* space <*> tyP

-- | Parse a Prog
progP :: Parser Prog
progP = Prog <$> many (tdeclP <* many newline) <*> many (globalP <* many newline) <*> many1 (fdeclP <* many newline)
