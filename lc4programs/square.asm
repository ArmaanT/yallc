;; Expected: 25
square
	ADD R6, R6, #-3
	STR R7, R6, #1
	STR R5, R6, #0
	ADD R5, R6, #0
	LDR R0, R5, #3
	ADD R6, R6, #-1
	STR R0, R6, #0
	LDR R0, R6, #0
	ADD R6, R6, #-1
	STR R0, R6, #0
	LDR R0, R6, #0
	LDR R1, R6, #1
	MUL R0, R0, R1
	ADD R6, R6, #1
	STR R0, R6, #0
	LDR R7, R6, #0
	STR R7, R5, #2
	ADD R6, R5, #0
	LDR R5, R6, #0
	LDR R7, R6, #1
	ADD R6, R6, #3
	RET
main
	ADD R6, R6, #-3
	STR R7, R6, #1
	STR R5, R6, #0
	ADD R5, R6, #0
	CONST R0, #5
	ADD R6, R6, #-1
	STR R0, R6, #0
	JSR square
	ADD R6, R6, #-1
	LDR R7, R6, #0
	STR R7, R5, #2
	ADD R6, R5, #0
	LDR R5, R6, #0
	LDR R7, R6, #1
	ADD R6, R6, #3
	RET
