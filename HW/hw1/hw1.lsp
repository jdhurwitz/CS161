;;;; Jonathan Hurwitz
;;;; 804258351
;;;; CS161 hw1


; TREE-CONTAINS (1)
; (> first second) = first > second
; Check to see if the arg n is in the tree. N = # and tree = ordered tree

(defun TREE-CONTAINS (n tree)
	(cond
		((null tree) nil)
		((listp tree) 
		 (cond
			((> n (second tree)) (TREE-CONTAINS n (third tree))) ; N > midpoint, go right
			((< n (second tree)) (TREE-CONTAINS n (first tree))) ; N < midpoint, go left
		 )
		)
		(t (equal n tree))
	)
)

; TREE-MAX (2) 
; Takes in tree -> max number in tree

(defun TREE-MAX (tree)
	(cond 
		((null tree) nil)
		((numberp tree) tree)
		((listp tree)
			(cond
				((equal (length tree) 1) (car tree))
				(t (TREE-MAX (car(last(cdr tree)))))
			)
		)
	)
)


; TREE-ORDER (3)
; Takes in ordered tree -> in-ordered list of numbers

(defun TREE-ORDER (tree)
	(cond 
		((null tree) nil)
		((numberp tree) (cons tree nil))
		((listp tree)
			(append
				(TREE-ORDER (first tree))
				(TREE-ORDER (second tree))
				(TREE-ORDER (caddr tree))
			)
		)
	)
)

; SUB-LIST (4)
; START and LEN are nonnegative integers.
; We want to return the sub=list of L startin gat position START w/ length LEN 
; The list is zero-indexed (first in L is position 0).

(defun SUB-LIST (L START LEN)
	(cond
		((equal LEN 0) ())
		((> LEN (length L)) nil)
		((and (equal START 0) (equal 1 LEN)) (list (first L)))

		((> START 0) (SUB-LIST (rest L) (- START 1) LEN))
		((equal START 0) (append (list (first L)) (SUB-LIST (rest L) 0 (- LEN 1))))


	)

)


; SPLIT-LIST (5)
; Takes a list L -> lists L1 and L2
; L is result of appending L1 and L2: (a b c d e) -> (a b) (c d e) 
;									  (a b c d)   -> (a b) (c d)
; Length of (L2 - L1) = 0 or 1, meaning extra characters go to L2

(defun SPLIT-LIST (L)
	(cond
		((null L) nil)
		((evenp (length L)) 
			(append (list(SUB-LIST L 0 (/ (length L) 2)) )
			   		(list(SUB-LIST L (/ (length L) 2) (/ (length L) 2))) )
		)

		((oddp (length L))
			(let* ( (x (/ (- (length L) 1) 2)) )
			(append (list(SUB-LIST L 0 x ))
					(list(SUB-LIST L x (- (length L) x) ))

			)
		))
	)
)


; Binary Trees: each node has 0 or 2 children.
; Leaf: 0 children
; Internal node: 2 children

; Leaf N is represented by atom N
; Internal node N is represented by list (L R) - L and R are left and right children

;BTREE-HEIGHT (6)
; Takes a binary tree -> returns height
; Height is defined as the length of the longest path from root to the farthest leaf node

(defun BTREE-HEIGHT (tree)
	(cond
		((atom tree) 0)
		((listp tree)
			(let* ( (l_res (+ (BTREE-HEIGHT (first tree)) 1)) 
					(r_res (+ (BTREE-HEIGHT (car(rest tree))) 1)))

				(cond
					((> l_res r_res) l_res)
					((> r_res l_res) r_res)
					((equal l_res r_res) l_res)
				)
			)
		)
	)
)


; LIST2BTREE (7)
; Takes non-empty list of atoms LEAVES -> binary tree
; Tree leaves are elements of LEAVES
; For any internal (non-leaf) node: (# leaves in R) - (# leaves in L) = 0 or 1
; 	-> The right tree will hold the extra atom in the odd case

(defun LIST2BTREE (LEAVES)
	(cond
		((equal (length LEAVES) 1) (car LEAVES)) 		;case for singleton list
		((equal (length LEAVES) 2) LEAVES)				;case for list of two
		((> (length LEAVES) 2) 
				(cons (LIST2BTREE (car(SPLIT-LIST LEAVES) ))
						(list(LIST2BTREE (cadr(SPLIT-LIST LEAVES) )))
				)
		)
		
	)
)


; BTREE2LIST (8)
; Binary tree TREE -> List of atoms
; Leaf: 0 children
; Internal node: 2 children

; Leaf N is represented by atom N
; Internal node N is represented by list (L R) - L and R are left and right children


(defun BTREE2LIST (TREE)
	(cond
		((atom TREE) (list TREE))
		((and (listp TREE) (and (atom (car TREE)) (atom (cadr TREE)))) TREE)
		(t 
			(append
				(BTREE2LIST (car TREE))
				(BTREE2LIST (cadr TREE))
			)
		)
	)
)


; IS-SAME (9)
; Takes in expressions E1 and E2 -> T if expressions are equal
; 	->E1 and E2 have atoms with all numbers
; LISP expression: either an atom or a list of LISP expressions 

(defun IS-SAME (E1 E2)
	(cond
		((or
			(and (null E1) (not(null E2)))
			(and (not(null E1)) (null E2))
		) nil)

		((and (null E1) (null E2)) t)
		((and (numberp (first E1)) (numberp (first E2)))
			(and
				(= (first E1) (first E2))
				(IS-SAME (rest E1) (rest E2))
			 t)
		)

		((and (listp (first E1)) (listp (first E2))) 
			(and 
				(IS-SAME (first E1) (first E2))
				(IS-SAME (rest E1)  (rest E2))
				
			t)

		)

		
	)
)