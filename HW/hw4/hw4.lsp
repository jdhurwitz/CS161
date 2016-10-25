;;Jonathan Hurwitz
;;804258351

;;CS161 hw4 - SAT

; Conjunction: terms are ANDed
; Disjunctino: terms are ORed

; A propositional sentence is a CNF iff it is a conjunction of clauses. Each clause is a disjunction of literals.

; ( lit OR lit ) AND lit AND (lit OR lit)

; A clause is a disjunction of literals
; CNF is satisfiable if a complete variable assignment satisfying each clause of the CNF exists

; Each clause of the CNF represents a constraint of the CSP.

; If a clause contains a single value, then this value is a constraint
; we must satisfy. For example:
;	((1 -2 3) (-1) (-2 -3))
; 	(-1) means that we have !1, so 1 must be false in order for -1 to be true.






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

; CREATE-DOMAINS
; Creates the initial domain state list
; EX: For 3 variables, we have a list (1 2 3)
;	 The init domains will all have T and NIL as states
;		( (1 (T NIL)) (2 (T NIL)) (3 (T NIL)) )

; Input: n (number of variables)
; Output: DOMAIN_STATE, a list of domain lists

(defun CREATE-DOMAINS (n)
	(let ((INIT (append (list T) (list NIL))))
		(cond
			((equal n 0) NIL)
			(t
				(append
					(list(append (list n) (list INIT)))
					(CREATE-DOMAINS (- n 1))
				)
			)
		)
	)
)

; GET-DOMAIN
; After we assign a variable, we need to check to see if it affects domains of other vars.
; Input: current domains
; Output: 
;	If updated domain is valid		 -> updated domain
;	Else if updated domain invalid   -> NIL, representing that we can't enforce this 
;											variable assignment


; Choose a variable to assign:
;		-If unary characters exist AND domain = 2, assign this
;		-Else assign character that occurs most often (in clauses)
;			-If all characters occur same amount, choose first one -> assign T
; After assignment, updated the affected domains of other vars.
; Check to see if any domains are NIL -> return NIL
; If not:
;		-Choose first var w/ domain with size 2 (T, F) (means it hasn't been assigned yet)

(defun solve (CUR_DOMAIN CNF)
	(cond
		((CHECK-FOR-EMPTY CUR-DOMAIN) NIL) 	;This path doesn't work

		;If we have a unary character AND domain = 2, assign the first one

	)
)

; GET-ASSIGNABLE-UNARY
; Finds the first unary with a non-empty domain
(defun GET-ASSIGNABLE-UNARY (UNARY_LIST DOMAINS)
	(cond
		((null UNARY_LIST) NIL)
		((IS-NOT-EMPTY (abs(first UNARY_LIST)) DOMAINS)  
			(first UNARY_LIST)
		)

		(T
			(GET-ASSIGNABLE-UNARY (rest UNARY_LIST) DOMAINS)
		)
	)
)

; IS-NOT-EMPTY
; Checks to see if a value (variable) corresponds to an empty domain
(defun IS-NOT-EMPTY (val DOMAINS)
	(cond
		((null DOMAINS) NIL)
		((equal (first(first DOMAINS)) val)
			(cond
				;If the domains are empty
				((null (second(first DOMAINS))) NIL)


				;If the domains are not empty, IS-NOT-EMTPY should return T
				(T 
					T
				)
			)
		)
		(T
			(IS-NOT-EMPTY val (rest DOMAINS))
		)
	)
)


; Find unary variables in the CNF
; Return absolute value
(defun find-unary (CNF)
	(cond
		((null CNF) NIL)
		(t
			(cond
				((is-unary (first CNF))
					(append
						;The absolute value of the variable
						;(list (abs (car(first CNF))))
						(first CNF)
						(find-unary (rest CNF))
					)
				)

				; The list has more than one variable in it, so we skip and keep checking 
				; the rest of the CNF
				(t
					(find-unary (rest CNF))
				)
			)
		)
	)

)

; Check to see if a list only has 1 member
(defun is-unary (L)
	(cond
		((null L ) NIL)
		(t
			(equal (length L) 1)
		)
	)
)

; CHECK-FOR-EMPTY
; Checks a list of lists (the domain lists) for an empty list.
; If empty list exists -> T
; Else -> NIL
(defun CHECK-FOR-EMPTY (DOMAINS)
	(cond
		((null DOMAINS) NIL)
		((equal (length (second(first DOMAINS))) 0) T)
		(t 
			(CHECK-FOR-EMPTY (rest DOMAINS))
		)
	)
)

; SAT-SOLVER
(defun sat? (n delta)
	(let (INIT_DOMAINS (CREATE-DOMAINS n))

	)

)
