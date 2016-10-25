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




; DOM-TO-ASSN
; Convert a domain set to assignment
; EX: ((3 NIL)(2 T)(1 NIL)) -> (-1 2 -3)
; First need to sort DOM to look like this: ((1 NIL)(2 T)(3 NIL))
; This function must receive a list of variables with domains of size 1

(defun DOM-TO-ASSN (sorted)
	(cond
		((null sorted) NIL)
		(t
			(cond
				;We append the absolute value of the number 
				((equal (first(second(first sorted))) T)
					(append (list (abs(first(first sorted)))) 
							(DOM-TO-ASSN (rest sorted))
					)
				)

				;This means we need the negative of the var
				((equal (first(second(first sorted))) NIL)
					(append (list (- (first(first sorted)))) 
							(DOM-TO-ASSN (rest sorted))
					)
				)
			)
		)
	)
)


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
	(let ((unary (GET-ASSIGNABLE UNARY (FIND-UNARY CNF)))) 
		(cond
			((CHECK-FOR-EMPTY CUR-DOMAIN) NIL) 	;If any domain is NIL, no solution on path
			((null CUR_DOMAIN) NIL)				;No answer

			;Check to see if all variables have domain = 1
			((equal (NUM-FULL-DOMAINS CUR_DOMAIN) 0)
				;If so, we are ready to test to see if our assignments satsify the CNF
				(EVAL-CNF
					CNF
					(DOM-TO-ASSN (reverse CUR_DOMAIN)) 
				
				)
			)


			;If we have a unary character AND domain = 2, assign the first one	
			((not (equal unary NIL))	;means that var unary is some value that we can set
				;Set that value and get updated domains.
				;Pass the updated single value to check across all other domains 
				;in case a constraint needs to be forced


				(UPDATE-RELATED-DOMAINS 
					;Returns an updated domain for the unary var
					(UPDATE-SINGLE-DOMAIN unary CUR_DOMAIN)

					CNF
				)

			)

			;Otherwise, we find the next var with domain size = 2 and choose T
		)
	)
)

; UPDATE-RELATED-DOMAINS
; If we change a variable and it forces a constraint on other variables, we have to check
; those domains and update them accordingly.
;
; A constraint will be imposed on a clause by 1 variable to others if by changing it,
; a condition is imposed on another variable in a clause. This occurs if there is only
; one free variable left to change.
; 
; Therefore, in each clause, we check:
;	-If there is more than 1 variable
;		-If so, we check to see # of variables with domain of size 2
;			-1 variable means we need to fit a constraint and check the CNF
;			- >1 variable means we leave those variables be

(defun UPDATE-RELATED-DOMAINS (updated_domains CNF)
	(cond
		((null CNF) updated_domains)

		;Check clause to see if there is more than 1 var
		((> (length (first CNF)) 1)

			;Check to see # variables w/ domain of size 2
			(cond
				((equal (NUM-FULL-DOMAINS updated_domains) 1)
					;We need to figure out how to make this clause true
					;GET-VAR-DOM2 gets the domain with size 2
					(cond
						;Try with var = T
						((EVAL-CNF CNF
							(DOM-TO-ASSN
								(reverse(UPDATE-SINGLE-DOMAIN 
									(GET-VAR-DOM2 updated_domains) updated_domains)
								)
							)
						) 
							;Return the updated domain
							(UPDATE-SINGLE-DOMAIN 
									(GET-VAR-DOM2 updated_domains) updated_domains)
								
							
						)

						;Try with var = NIL
						((EVAL-CNF CNF
							(DOM-TO-ASSN
								(reverse(UPDATE-SINGLE-DOMAIN 
									(- (GET-VAR-DOM2 updated_domains)) updated_domains)
								)
							)
						) 
							;Return the updated domain
							(UPDATE-SINGLE-DOMAIN 
									(- (GET-VAR-DOM2 updated_domains)) updated_domains)
								
							
						)

						;Neither of these work, so this is UNSAT
						(t
							NIL
						)

					)
				)
				((> (NUM-FULL-DOMAINS updated_domains) 1)
					(UPDATE-RELATED-DOMAINS updated_domains (rest CNF))
				)
			)
		)

		;If there is not more than 1 var, we move on to the rest of the CNF
		(t
			(UDPATE-RELATED-DOMAINS updated_domains (rest CNF))
		)
	)

)

; UPDATE-SINGLE-DOMAIN
; Updates a single variable's domain and returns that set of domains.
; Input: Var to update, current domain set DOMAINS
; Output: updated state of domains
(defun UPDATE-SINGLE-DOMAIN (var DOMAINS)
	(cond
		((null DOMAINS) DOMAINS)
		((equal (abs var) (first(first DOMAINS)))
			(cond
				;If the variable is negative, we set it to false and make its domain false
				((< var 0)
					(append (list(append (list(abs var)) (list(list NIL))))
							(UPDATE-SINGLE-DOMAIN var (rest DOMAINS)))
				)
				;If the variable is positive, then to make it true we set it to be true
				((> var 0)
					(append (list(append (list(abs var)) (list(list T))))
							(UPDATE-SINGLE-DOMAIN var (rest DOMAINS)))
				)
			)
		)
		(t
			(append (list(first DOMAINS)) (UPDATE-SINGLE-DOMAIN var (rest DOMAINS)))
		)
	)
)



; GET-ASSIGNABLE-UNARY
; Finds the first unary with a non-empty domain
(defun GET-ASSIGNABLE-UNARY (UNARY_LIST DOMAINS)
	(cond
		((null UNARY_LIST) NIL)
		((IS-ASSIGNABLE  (abs(first UNARY_LIST)) DOMAINS)  
			(first UNARY_LIST)
		)

		(T
			(GET-ASSIGNABLE-UNARY (rest UNARY_LIST) DOMAINS)
		)
	)
)

; NUM-FULL-DOMAINS
; Gets the number of domains with size 2
(defun NUM-FULL-DOMAINS (DOMAINS)
	(let ((first_dom (second(first DOMAINS))))
		(cond 
			((null DOMAINS) 0)
			((equal (length first_dom) 2)
				(+ 1 (NUM-FULL-DOMAINS (rest DOMAINS)))
			)
			(t
				(NUM-FULL-DOMAINS (rest DOMAINS))
			)
		)
	)
)

; GET-VAR-DOM2
; Get first variable corresponding to domain of size 2 
(defun GET-VAR-DOM2 (domains)
	(cond
		((null domains) NIL)
		((equal (length (second(first domains))) 2) 
			(first (first domains))
		)
		(t
			(GET-VAR-DOM2 (rest domains))
		)
	)
)

; IS-ASSIGNABLE
; Checks to see if a value (variable) corresponds to a domain that has 2 values
(defun IS-ASSIGNABLE  (val DOMAINS)
	(cond
		((null DOMAINS) NIL)
		((equal (first(first DOMAINS)) val)
			(cond
				;If the domains are empty
				((< (length (second(first DOMAINS))) 2) NIL)


				;If the domains are not empty, IS-NOT-EMTPY should return T
				(T 
					(equal (length (second(first DOMAINS))) 2)

				)
			)
		)
		(T
			(IS-ASSIGNABLE  val (rest DOMAINS))
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
