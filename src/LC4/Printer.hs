{-# LANGUAGE FlexibleInstances #-}

module LC4.Printer where

import Data.Char (toLower)
import LC4.Ast
  ( Asm (..),
    Condition,
    Elem (Elem),
    Imm (..),
    Instruction (..),
    Lbl,
    Program (..),
    Register,
  )
import PP (PP (..))
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

instance PP Lbl where
  pp l = PP.text l

instance PP Register where
  pp r = PP.text $ show r

instance PP Imm where
  pp (Imm imm) = PP.char '#' <> PP.int imm

instance PP Condition where
  pp c = PP.text $ map toLower $ show c

addComma :: (PP a) => a -> Doc
addComma a = pp a <> PP.comma

formatTwoInstruction :: (PP a, PP b) => String -> a -> b -> Doc
formatTwoInstruction opcode a b = PP.hsep [PP.text opcode, addComma a, pp b]

formatThreeInstruction :: (PP a, PP b, PP c) => String -> a -> b -> c -> Doc
formatThreeInstruction opcode a b c = PP.hsep [PP.text opcode, addComma a, addComma b, pp c]

instance PP Instruction where
  pp NOP = PP.text "NOP"
  pp (BR c l) = (PP.text "BR" <> pp c) PP.<+> pp l
  pp (ADD r1 r2 r3) = formatThreeInstruction "ADD" r1 r2 r3
  pp (MUL r1 r2 r3) = formatThreeInstruction "MUL" r1 r2 r3
  pp (SUB r1 r2 r3) = formatThreeInstruction "SUB" r1 r2 r3
  pp (DIV r1 r2 r3) = formatThreeInstruction "DIV" r1 r2 r3
  pp (ADDI r1 r2 imm) = formatThreeInstruction "ADD" r1 r2 imm
  pp (CMP r1 r2) = formatTwoInstruction "CMP" r1 r2
  pp (CMPU r1 r2) = formatTwoInstruction "CMPU" r1 r2
  pp (CMPI r imm) = formatTwoInstruction "CMPI" r imm
  pp (CMPIU r imm) = formatTwoInstruction "CMPIU" r imm
  pp (JSRR r) = PP.text "JSRR" PP.<+> pp r
  pp (JSR lbl) = PP.text "JSR" PP.<+> pp lbl
  pp (AND r1 r2 r3) = formatThreeInstruction "AND" r1 r2 r3
  pp (NOT r1 r2) = formatTwoInstruction "NOT" r1 r2
  pp (OR r1 r2 r3) = formatThreeInstruction "OR" r1 r2 r3
  pp (XOR r1 r2 r3) = formatThreeInstruction "XOR" r1 r2 r3
  pp (ANDI r1 r2 imm) = formatThreeInstruction "AND" r1 r2 imm
  pp (LDR r1 r2 imm) = formatThreeInstruction "LDR" r1 r2 imm
  pp (STR r1 r2 imm) = formatThreeInstruction "STR" r1 r2 imm
  pp RTI = PP.text "RTI"
  pp (CONST r imm) = formatTwoInstruction "CONST" r imm
  pp (SLL r1 r2 imm) = formatThreeInstruction "SLL" r1 r2 imm
  pp (SRA r1 r2 imm) = formatThreeInstruction "SRA" r1 r2 imm
  pp (SRL r1 r2 imm) = formatThreeInstruction "SRL" r1 r2 imm
  pp (MOD r1 r2 r3) = formatThreeInstruction "MOD" r1 r2 r3
  pp (JMPR r) = PP.text "JMPR" PP.<+> pp r
  pp (JMP lbl) = PP.text "JMP" PP.<+> pp lbl
  pp (HICONST r imm) = formatTwoInstruction "HICONST" r imm
  pp (TRAP imm) = PP.text "TRAP" PP.<+> pp imm
  pp RET = PP.text "RET"
  pp (LEA r lbl) = formatTwoInstruction "LEA" r lbl
  pp (LC r lbl) = formatTwoInstruction "LC" r lbl

instance PP Asm where
  pp (Text ins) = PP.vcat (map pp ins)
  pp (Data vals) = PP.vcat (map ppFill vals)
    where
      ppFill :: Imm -> Doc
      ppFill i = PP.text ".FILL" PP.<+> pp i

instance PP Elem where
  pp (Elem lbl asm) = (PP.$$) (PP.text lbl) (PP.nest 7 (pp asm))

instance PP Program where
  pp (Program elems) = PP.vcat (map pp elems)
