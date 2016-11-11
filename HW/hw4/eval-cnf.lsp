; EVAL-CLAUSE
; Takes the diff of the assignments and a clause
; If at least 1 term matches its corresponding assignment value, return T
; Else we get to the end of the assignemnts -> all vars are false

; Example: clause is (1 -2 3)  ||| Assignment is (-1 -2 -3)
;			-> returns T because -2 and -2 match to return a T
(defun EVAL-CLAUSE (C Asmts)
	(cond
		((or (null C) (null Asmts)) NIL)
		((is-present (first C) Asmts) T)
		(t
			(EVAL-CLAUSE (rest C) Asmts)
		)
	)
)

; IS-PRESENT
; Check to see if an element is in a list
; Inputs: element E, list L
(defun is-present (E L)
	(cond
		((null L) NIL)
		((equal (first L) E) T)
		(t
			(is-present E (rest L))
		)
	)
)

; EVAL-CNF
; Check to see if the CNF returns T/NIL for a given assignment
; Inputs: CNF, variable assignment

(defun EVAL-CNF (CNF Asmt)
	(cond
		((null CNF) T)
		(t
			(and
				(eval-clause (first CNF) Asmt)
				(EVAL-CNF (rest CNF) Asmt)
			)
		)
	)
)