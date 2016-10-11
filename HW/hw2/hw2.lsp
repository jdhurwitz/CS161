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