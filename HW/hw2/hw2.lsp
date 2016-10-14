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




;------------Missionary Cannibals Problem------------
; These functions implement a depth-first iterative-deepening solver for the
; missionary-cannibal problem. In this problem, three missionaries and three
; cannibals are trying to go from the east side of a river to the west side.
; They have a single boat that can carry two people at a time from one side of
; the river to the other. There must be at least one person in the boat to cross
; the river. There can never be more cannibals on one side of the river than
; missionaries. If there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list (MISSIONARIES
; CANNIBALS SIDE). SIDE represents which side the boat is currently on, and is T
; if it is on the east side and NIL if on the west side. MISSIONARIES and
; CANNIBALS represent the number of missionaries and cannibals on the same side
; as the boat. Thus, the initial state for this problem is (3 3 T) (three
; missionaries, three cannibals, and the boat are all on the east side of the
; river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function ID-DFS, which is called
; with the initial state to search from and the depth up to which depth-first
; search will be performed. It returns the complete path from the initial state
; to the goal state: this path is a list of intermediate problem states. The
; first element of the path is the initial state and the last element is the
; goal state. Each intermediate state is the state that results from applying
; the appropriate operator to the preceding state.

; To solve the original problem, one would call (ID-DFS '(3 3 T) 0). 

; Examples of calls to some of the helper functions can be found after the code.


; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.

(defun final-state (s)
	(cond
		(
			(and
				(equal (first s) 3)
				(equal (second s) 3)
				(equal (third s) nil)
			)
		)
		(t NIL)
	)
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).

(defun next-state (s m c)
	(cond
		((> m (first s)) nil)
		((> c (second s)) nil)

		;For the side we're currently on:
		;	-> fail if a missionary exists and cannibals > missionaries
		((and 
			(> (- (first s) m) 0) 
			(> (- (second s) c) (- (first s) m) )) 
		NIL)

		;We know that there are 3-var on the other side
		((and 
			(> (+ (- 3 (first s)) m) 0) 
			(> (+ (- 3 (second s)) c) (+ (- 3 (first s)) m))
		) NIL)



		;Else, it is valid and we create the return state
		(t  
			 (list(list
				 (+ (- 3 (first s)) m)
				 (+ (- 3 (second s)) c) 
				 (not(third s))
			 ))
		)
	)
	
)



; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.

; (s) = (m c t/nil)
(defun succ-fn (s)
	(append
		;We append all possible next-states because if they aren't valid, 
		; the (append nil _) will just return a list with (_)

		(next-state s 0 1)
		(next-state s 1 0)
		(next-state s 1 1)
		(next-state s 2 0)
		(next-state s 0 2)
	)

)

; MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
; path from the initial state to the current state (PATH), the legal successor
; states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a single depth-first iteration to the given depth on
; each element of STATES in turn. If any of those searches reaches the final
; state, MULT-DFS returns the complete path from the initial state to the goal
; state. Otherwise, it returns NIL.


(defun mult-dfs (states path depth)
	(cond
		((null states) NIL)

		;We try a single DFS on the first state. If it doesn't return nil, we return that
		((not(equal (SINGLE-DFS (first states) path depth) nil)) 
			(SINGLE-DFS (first states) path depth)
		)

		;Else we need to go to the rest of the states in the list "states"
		(t (MULT-DFS (rest states) path depth))

	)
)



; SINGLE-DFS does a single depth-first iteration to the given depth. It takes
; three arguments: a state (S), the path from the initial state to S (PATH), and
; the depth (DEPTH). If S is the initial state in our search, PATH should be
; NIL. It performs a depth-first search starting at the given state. It returns
; the path from the initial state to the goal state, if any, or NIL otherwise.

(defun single-dfs (s path depth)
	(cond
		((and
			(equal depth 0)
			(not(final-state s))
		) NIL)

		 ;we hit a final state and can add our goal to the total path
		((final-state s) 
			(append path (list s))
		)

		;we want to explore the other states, so we need to generate successors
		;we can process the set of possible successors by using mult
		(t
			(mult-dfs
				(succ-fn s)
				(append path (list s))
				(- depth 1)
			)
		)
	)
)




; ID-DFS is the top-level function. It takes two arguments: an initial state (S)
; and a search depth (DEPTH). ID-DFS performs a series of depth-first
; iterations, starting from the given depth until a solution is found. It
; returns the path from the initial state to the goal state. The very first call
; to ID-DFS should use depth = 0.

(defun id-dfs (s depth)
	;(print depth)
	(cond
		(
			;If single DFS doesn't return NIL, we have a result
			(not(equal (single-dfs s nil depth) NIL)) 
				(single-dfs s nil depth)

		)

		;else increase depth and keep searching
		(t 
			(id-dfs s (+ depth 1))
		)
	)
)

; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (note that NEXT-STATE
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; SUCC-FN returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))



