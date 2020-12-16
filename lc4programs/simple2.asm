;; Expected: 1
main
	ADD R6, R6, #-3
	STR R7, R6, #1
	STR R5, R6, #0
	ADD R5, R6, #0
	CONST R0, #88
	HICONST R0, #27
	ADD R6, R6, #-1
	STR R0, R6, #0
	CONST R0, #190
	HICONST R0, #252
	ADD R6, R6, #-1
	STR R0, R6, #0
	LDR R0, R6, #0
	LDR R1, R6, #1
	CMP R0, R1
	BRn main_test_simple2_j_cmp_true_0
	CONST R0, #0
	STR R0, R6, #1
	BRnzp main_test_simple2_j_cmp_end_0
main_test_simple2_j_cmp_true_0
	CONST R0, #1
	STR R0, R6, #1
main_test_simple2_j_cmp_end_0
	ADD R6, R6, #1
	LDR R7, R6, #0
	STR R7, R5, #2
	ADD R6, R5, #0
	LDR R5, R6, #0
	LDR R7, R6, #1
	ADD R6, R6, #3
	RET
