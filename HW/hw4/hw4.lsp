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


; Find unary variables in the CNF
(defun find-unary (CNF)
	(cond
		((null CNF) NIL)
		(t
			(cond
				((is-unary (first CNF))
					(append
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


; GET-DOMAIN
; After we assign a variable, we need to check to see if it affects domains of other vars.
; Input: current domains
; Output: 
;	If updated domain is valid		 -> updated domain
;	Else if updated domain invalid   -> NIL, representing that we can't enforce this 
;											variable assignment

