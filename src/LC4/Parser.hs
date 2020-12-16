module LC4.Parser where

import LC4.Ast
  ( Asm (Text),
    Condition (..),
    Elem (Elem),
    Imm (..),
    Instruction (..),
    Program (..),
    Register (..),
  )
import Parser (commaSpaceP, idP, intP)
import Text.Parsec
  ( char,
    many,
    newline,
    space,
    spaces,
    string,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)

registerP :: Parser Register
registerP =
  try (R0 <$ string "R0")
    <|> try (R1 <$ string "R1")
    <|> try (R2 <$ string "R2")
    <|> try (R3 <$ string "R3")
    <|> try (R4 <$ string "R4")
    <|> try (R5 <$ string "R5")
    <|> try (R6 <$ string "R6")
    <|> try (R7 <$ string "R7")

immP :: Parser Imm
immP = Imm <$> (char '#' *> intP)

conditionP :: Parser Condition
conditionP =
  try (NZP <$ string "nzp")
    <|> try (NZ <$ string "nz")
    <|> try (NP <$ string "np")
    <|> try (N <$ string "n")
    <|> try (ZP <$ string "zp")
    <|> try (Z <$ string "z")
    <|> try (P <$ string "p")

threeInstructionParser :: (a1 -> a2 -> a3 -> Instruction) -> String -> Parser a1 -> Parser a2 -> Parser a3 -> Parser Instruction
threeInstructionParser op s p1 p2 p3 = op <$> (string s *> space *> p1 <* commaSpaceP) <*> p2 <* commaSpaceP <*> p3

twoInstructionParser :: (a1 -> a2 -> Instruction) -> String -> Parser a1 -> Parser a2 -> Parser Instruction
twoInstructionParser op s p1 p2 = op <$> (string s *> space *> p1 <* commaSpaceP) <*> p2

oneInstructionParser :: (a1 -> Instruction) -> String -> Parser a1 -> Parser Instruction
oneInstructionParser op s p1 = op <$> (string s *> space *> p1)

instructionP :: Parser Instruction
instructionP =
  try (NOP <$ string "NOP")
    <|> (BR <$> (string "BR" *> conditionP <* space) <*> idP)
    <|> try (threeInstructionParser ADD "ADD" registerP registerP registerP)
    <|> try (threeInstructionParser MUL "MUL" registerP registerP registerP)
    <|> try (threeInstructionParser SUB "SUB" registerP registerP registerP)
    <|> try (threeInstructionParser DIV "DIV" registerP registerP registerP)
    <|> try (threeInstructionParser ADDI "ADD" registerP registerP immP)
    <|> try (twoInstructionParser CMP "CMP" registerP registerP)
    <|> try (twoInstructionParser CMPU "CMPU" registerP registerP)
    <|> try (twoInstructionParser CMPI "CMPI" registerP immP)
    <|> try (twoInstructionParser CMPIU "CMPIU" registerP immP)
    <|> try (oneInstructionParser JSRR "JSRR" registerP)
    <|> try (oneInstructionParser JSR "JSR" idP)
    <|> try (threeInstructionParser AND "AND" registerP registerP registerP)
    <|> try (twoInstructionParser NOT "NOT" registerP registerP)
    <|> try (threeInstructionParser OR "OR" registerP registerP registerP)
    <|> try (threeInstructionParser XOR "XOR" registerP registerP registerP)
    <|> try (threeInstructionParser ANDI "AND" registerP registerP immP)
    <|> try (threeInstructionParser LDR "LDR" registerP registerP immP)
    <|> try (threeInstructionParser STR "STR" registerP registerP immP)
    <|> try (RTI <$ string "RTI")
    <|> try (twoInstructionParser CONST "CONST" registerP immP)
    <|> try (threeInstructionParser SLL "SLL" registerP registerP immP)
    <|> try (threeInstructionParser SRA "SRA" registerP registerP immP)
    <|> try (threeInstructionParser SRL "SRL" registerP registerP immP)
    <|> try (threeInstructionParser MOD "MOD" registerP registerP registerP)
    <|> try (oneInstructionParser JMPR "JMPR" registerP)
    <|> try (oneInstructionParser JMP "JMP" idP)
    <|> try (twoInstructionParser HICONST "HICONST" registerP immP)
    <|> try (oneInstructionParser TRAP "TRAP" immP)
    <|> try (RET <$ string "RET")
    <|> try (twoInstructionParser LEA "LEA" registerP idP)
    <|> try (twoInstructionParser LC "LC" registerP idP)

asmP :: Parser Asm
asmP = Text <$> many (try $ spaces *> instructionP <* (newline <|> pure ' '))

elemP :: Parser Elem
elemP = Elem <$> idP <*> ((try newline <|> pure '_') *> asmP)

programP :: Parser Program
programP = Program <$> many elemP
