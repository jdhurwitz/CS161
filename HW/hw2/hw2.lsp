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

