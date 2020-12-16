module Programs where

import LC4
  ( Asm (Text),
    Elem (Elem),
    Imm (Imm),
    Instruction (ADD, ADDI, LDR, RET, STR),
    Program (..),
    Register (R0, R1, R5, R6, R7),
  )
import LL
  ( Block (Block),
    Bop (Add),
    Fdecl (Fdecl),
    Insn (Binop),
    Operand (Id),
    Prog (Prog),
    Terminator (Ret),
    Ty (I64),
  )

-- define i64 @addTwo(i64 %x, i64 %y) {
--   %1 = add i64 %x, %y
--   ret i64 %1
-- }
llAddTwo :: LL.Prog
llAddTwo =
  Prog
    []
    []
    [ ( "addTwo",
        Fdecl
          ([I64, I64], I64)
          ["x", "y"]
          ( Block
              [ ( "1",
                  Binop Add I64 (Id "x") (Id "y")
                )
              ]
              ("2", Ret I64 (Just (Id "1"))),
            []
          )
      )
    ]

lc4AddTwo :: LC4.Program
lc4AddTwo =
  Program
    [ Elem
        "addTwo"
        ( Text
            [ -- Prologue
              ADDI R6 R6 (Imm (-3)), -- Add 3 spots to stack for (FP, RA, RV)
              STR R7 R6 (Imm 1), -- Save RA
              STR R5 R6 (Imm 0), -- Save FP
              ADDI R5 R6 (Imm 0), -- Set local FP
              ADDI R6 R6 (Imm (-1)), -- Allocate space for 1 variable
              -- AddTwo (Body)
              LDR R0 R5 (Imm 3), -- Loads x into R0
              LDR R1 R5 (Imm 4), -- Loads y into R1
              ADD R0 R0 R1, -- Add x + y and store in R0
              STR R0 R5 (Imm (-1)), -- Set %1 equal to x + y
              -- Set return value
              LDR R0 R5 (Imm (-1)), -- Load %1 into R0
              STR R0 R5 (Imm 2), -- Store %1 in RV
              -- Epilogue
              ADDI R6 R5 (Imm 0), -- Set SP to be FP (Pop local stack space)
              LDR R5 R6 (Imm 0), -- Load previous FP
              LDR R7 R6 (Imm 1), -- Load previous RA
              ADDI R6 R6 (Imm 3), -- Free space used by FP, RA, RV
              RET
            ]
        )
    ]
