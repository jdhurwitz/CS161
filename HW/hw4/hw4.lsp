; Jonathan Hurwitz
; 804258351


;;CS161 hw4 - SAT

; Conjunction: terms are ANDed
; Disjunction: terms are ORed

; A propositional sentence is a CNF iff it is a conjunction of clauses. Each clause is a disjunction of literals.

; ( lit OR lit ) AND lit AND (lit OR lit)

; A clause is a disjunction of literals
; CNF is satisfiable if a complete variable assignment satisfying each clause of the CNF exists

; Each clause of the CNF represents a constraint of the CSP.

; If a clause contains a single value, then this value is a constraint
; we must satisfy. For example:
;	((1 -2 3) (-1) (-2 -3))
; 	(-1) means that we have !1, so 1 must be false in order for -1 to be true.


(defun sat? (n delta)
	(process-solution n 1 delta)
)

(defun process-solution (n init delta)
	(let* ((pre_proc_sol (smart-DFS delta init n))
		  (num_remaining (- n (length pre_proc_sol)))

		)
		(cond 
			((equal (length pre_proc_sol) 0) NIL)
			(T
				(populate-sol pre_proc_sol num_remaining)
			)
		)

	)
)


; smart-DFS will leave out all the variables that need to be set to T
; cur_sol: the solution lacking the rest of the variables
; vars_remaining: the # of variables we have left to add back in

(defun populate-sol (cur_sol vars_remaining)
	(cond
		((equal vars_remaining 0) cur_sol)
		(t
			(populate-sol 
				(append
					cur_sol
					(list (+ (length cur_sol) 1))
				)

				(- vars_remaining 1)
			)
		)
	)
)

; In the example ((1 -2 3)(-1)(-2 -3)) it's easy to see that assigning a variable (e.g. 1 = 1) is 
; the same thing as removing its inverse. So if we assign 1, we'd remove -1. 
; -In the special case of assigning 1, we see that removing -1 causes this to fail since it is
; a single element. We need to check for that.

; Concept
; -Check F first. If that branch isn't valid, we cut it off.
; -Check T second. If that branch isn't valid, T/F assignments for this variable don't work
; and we need to backtrack.
; Acts like smart DFS

; Inputs: 
; 		cnf: the cnf (delta)
;		cur_var: the current variable from (1...n) that we are processing
;		n: the number of variables in the problem

(defun smart-DFS (cnf cur_var n)
		(cond ((null cnf) NIL)

			  ;We've reached a single clause
			  ((atom cnf) (list cnf))

			  ;We've gone too far
			  ((> cur_var n) NIL)

			  ;Process the choice of taking T and the choice of taking F
		  	  (t (let ((T_branch (process-branch cnf cur_var))
		  	  	       (F_branch (process-branch cnf (- cur_var)))
		  	  	      )
		  	  	       
		  	  	    ;If we choose F and it returns NIL, ultimately this branch needs
		  	  	    ;to be pruned.
			  	  	(if (null F_branch)
						
				  	  		(let ((assnT (smart-DFS T_branch (+ cur_var 1) n))) 

				  	  			;No solution. Need to return NIL and backtrack
					  	  	 	(if (null T_branch) NIL
						  	  	 	(if (atom T_branch) (list cur_var)
				 
						  	  	 	(if (null assnT) NIL
						  	  			(append (list cur_var) assnT))
						  	  		)

					  	  		) 

				  	  		)

				  	  	 ;ELSE
				  	  	 ;If F_branch did not return NULL, we check to see if it is a single
				  	  	 ;element. If so, that we can assign it by flipping it.
				  	  	 (if (atom F_branch) (list (- cur_var)) 
					  	  	 (let ((assnF (smart-DFS F_branch (+ cur_var 1) n)))
						  	  	 (if (null assnF)
						  	  	 	(let ((assnT (smart-DFS T_branch (+ cur_var 1) n)))

						  	  	 	;No solution. Return NIL and backtrack.
						  	  	 	(if (null assnT) NIL
						  	  	 	    (append (list cur_var) assnT))) 
						  	  	 	    
						  	  	 	;Add this var (flip the bit to assign) to the solution.
						  	  	 	(append (list (- cur_var)) assnF)
						  	  	 ) 
						  	  )
					  	 )  
				  	)
		  	  	)
		  	  )
		)
)


; We process a branch and see if it is a valid path. We are assigning some number "cur_var" and 
; need to see if by assigning it we violate anything (creating a null list, for example, by removing
; its inverse).

(defun process-branch (cnf cur_var)
	(cond
		((null cnf) NIL)

		;We can't remove "cur_var" (check the negative)
		((null (is-valid-removal cnf (- cur_var))) NIL)

		;If it occurs in every list, it should be kept 
		((null (remove-with-var cnf cur_var)) 
			cur_var
		)

		;It's possible that the number doesn't appear in the lists (no constraint). We can just 
		;return the CNF
		((and
			(equal (remove-with-var cnf cur_var) cnf)
			(equal (remove-with-var cnf (- cur_var)) cnf)
		) cnf)

		;If this is true, we need to fail and explore the negative element
		((equal (remove-with-var cnf cur_var) cnf) 
			NIL
		)

		(T
			(is-valid-removal (remove-with-var cnf cur_var) (- cur_var))
		)
	)
)




; Given a variable, remove all lists in which it is observed
(defun remove-with-var (cnf cur_var)
	(remove-with-varh1 cnf cur_var NIL)
)

; Assemble output
(defun remove-with-varh1 (cnf cur_var output_cnf)
	(cond
		((null cnf) output_cnf)
		((is-in-list cur_var (first cnf)) 
			(remove-with-varh1 (rest cnf) cur_var output_cnf)
		)
		(T
			(remove-with-varh1 (rest cnf) cur_var (append output_cnf (list (first cnf)) ))

		)
	)
)

; Checks to see if a variable is in a list
(defun is-in-list (val L)
	(cond
		((null L) NIL)
		((equal (first L) val) T)
		(T
			(is-in-list val (rest L))
		)
	)
)





;Gets the result of removing an element from each clause if the CNF is still valid
(defun is-valid-removal (cnf cur_var)
	(cond
		((is-valid-cnf cnf cur_var)
			(remove-elem-all cnf cur_var)
		)
		(T
			NIL
		)
	)
)

;Removes an element from each clause
(defun remove-elem-all (cnf val)
	(cond
		((null cnf) NIL)
		(T
			(append
				(list (remove-element val (first cnf)))
				(remove-elem-all (rest cnf) val)
			)
		)
	)
)

;Checks to see if the CNF is still valid after an element is removed. If it is, we return the result
;of removing the variable.
(defun is-valid-cnf (cnf cur_var)
	(cond
		((null cnf) T)

		;This means we made the clause false by doing some assignment
		((equal (remove-element cur_var (first cnf)) NIL)
			NIL
		)
		(T
			(is-valid-cnf (rest cnf) cur_var)
		)

	)
)

;Removes an element from a list
(defun remove-element (val L)
	(cond
		((null L) NIL)
		((equal (first L) val) (remove-element val (rest L)))
		(t
			(append
				(list (first L))
				(remove-element val (rest L))
			)
		)
	)
)


;Functions to test correctness 
;--------------------------------------------------------------------------------------------
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
;--------------------------------------------------------------------------------------------