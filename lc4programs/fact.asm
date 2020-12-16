;; Expected: 5040
fact
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
	CONST R0, #1
	ADD R6, R6, #-1
	STR R0, R6, #0
	LDR R0, R6, #0
	LDR R1, R6, #1
	CMP R0, R1
	BRzp fact_j_cmp_true_0
	CONST R0, #0
	STR R0, R6, #1
	BRnzp fact_j_cmp_end_0
fact_j_cmp_true_0
	CONST R0, #1
	STR R0, R6, #1
fact_j_cmp_end_0
	ADD R6, R6, #1
	ADD R6, R6, #1
	LDR R0, R6, #-1
	BRz fact_j_else_0
	CONST R0, #1
	ADD R6, R6, #-1
	STR R0, R6, #0
	BRnzp fact_j_endif_0
fact_j_else_0
	LDR R0, R6, #0
	ADD R6, R6, #-1
	STR R0, R6, #0
	CONST R0, #1
	ADD R6, R6, #-1
	STR R0, R6, #0
	LDR R0, R6, #0
	LDR R1, R6, #1
	STR R0, R6, #1
	STR R1, R6, #0
	LDR R0, R6, #0
	LDR R1, R6, #1
	SUB R0, R0, R1
	ADD R6, R6, #1
	STR R0, R6, #0
	JSR fact
	ADD R6, R6, #-1
	LDR R0, R6, #0
	LDR R1, R6, #1
	STR R0, R6, #1
	STR R1, R6, #0
	ADD R6, R6, #1
	LDR R0, R6, #0
	LDR R1, R6, #1
	MUL R0, R0, R1
	ADD R6, R6, #1
	STR R0, R6, #0
fact_j_endif_0
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
	CONST R0, #7
	ADD R6, R6, #-1
	STR R0, R6, #0
	JSR fact
	ADD R6, R6, #-1
	LDR R7, R6, #0
	STR R7, R5, #2
	ADD R6, R5, #0
	LDR R5, R6, #0
	LDR R7, R6, #1
	ADD R6, R6, #3
	RET
