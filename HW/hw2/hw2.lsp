;;;; Jonathan Hurwitz
;;;; 804258351
;;;; CS161 HW2

; Problem 1

(defun DFS (L)
	(cond
		((atom L) (list L))  		; case for when there are no children
		((listp L)			; this is a DFS list
			(cond
				((equal (length L) 1) (DFS (first L)))
				(t (append (DFS (first L)) (DFS (rest L))))
			)
		)
	)

)

; Problem 2
; Depth-First Iterative-Deepening
; DFID: top level function taking two arguments
;	inputs: list representation of tree, integer for max depth
; 	output: a single top-level list of terminal nodes in order visited for IDFS
; Example: (dfid '((A (B)) C (D)) 3) -> (C A C D A B C D)
;
; Functions must work for trees of arbitrary depth and branching factor 
;	-> do not assume a priori that an upper bound exists on these parameters
; EX2: (A (B C) (D) (E (F G)))

; 

; Depth Limited Search
; DFS but we set a depth limit


(defun DLS (TREE MAX_LEV)
	(cond
		((null TREE) NIL) ;empty tree
		((< MAX_LEV 0) NIL)  ; -1 case, but < 0 in case input is bad. Return here

		;Check to see if we have an atom. Return as list to use in append:
		((atom TREE) (list TREE))

		;If we get here, it is a list. Check to see if the list only has 1 element.
		((equal (length TREE) 1) (DLS (first TREE) (- MAX_LEV 1)))

		;We have a list with more than 1 sub-list. 
		;first will not add parentheses, so MAX_LEV -= 1 is valid as the next value.
		;rest will add parentheses, so MAX_LEV must be the next value as well.

		(t (append
				(DLS (first TREE) (- MAX_LEV 1))
				(DLS (rest TREE) MAX_LEV)
		   )
		)

	)
)

(defun DFID (TREE MAX_D)
	(cond
		((null TREE) nil)
		((< MAX_D 0) nil) ; -1 case, but < 0 just in case input is bad

		(t
			(append

				;Recursively call DFID to reduce depths. Prefixes will be smaller 
				;and smaller substrings.
				(DFID TREE (- MAX_D 1))

				;The DLS of max level will show up as the last substring
				(DLS TREE MAX_D)
			)
		)

	)

)
