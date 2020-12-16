;; Expected: 6765
main   ADD R6, R6, #-3
       STR R7, R6, #1
       STR R5, R6, #0
       ADD R5, R6, #0
       CONST R0, #20
       CONST R2, #0
       CONST R3, #1
       ADD R0, R0, #-1
main_loop
       CMPI R0, #0
       BRnz main_final
       ADD R3, R2, R3
       SUB R2, R3, R2
       ADD R0, R0, #-1
       BRnzp main_loop
main_final
       ADD R1, R3, #0
       STR R3, R5, #2
       ADD R6, R5, #0
       LDR R5, R6, #0
       LDR R7, R6, #1
       ADD R6, R6, #3
       RET
