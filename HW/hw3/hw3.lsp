;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;


;Recursively check each row to see if there is a box that is not in a goal position (check for 2)
(defun goal-test (s)
	(cond
		((null s) t)

		;Get the first element of the state (a list) and check for a box
		; If box -> we fail
		; Else 	 -> we continue checking
		((box-exists (first s)) NIL)

		(T
			(goal-test (rest s))
		)
	)
)

(defun box-exists(L)
	(cond
		((null L) NIL) ;if the list is empty, no box exists
		((isBox (first L)) T)
		(T
			(box-exists (rest L))
		)
	)
)

;--------------------------------------------------------------------------------------------
; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.


; UP = 1
; DOWN = 2
; LEFT = 3
; RIGHT = 4
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (c (car pos))
	 (r (cadr pos))

     ;(result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
     (result (list (try-move s 1) (try-move s 2) (try-move s 3) (try-move s 4)))
     )
    (cleanUpList result);end
   );end let
 )


; TRYMOVE
; Inputs: state S, direction D
; Output: 
;			Move is valid -> state resulting from move
;			Move is invalid -> NIL

; Keeper cannot walk into a wall or box
; Keeper can push box if square in pushing direction is a goal or empty
; 	-> Only one box can be pushed in a step

(defun try-move (S D)
	(let* ((pos (getKeeperPosition S 0))
		(c (car pos))
		(r (cadr pos)))

		(cond
			;is-illegal will return T for an illegal move
			;				 return NIL for a legal move
			((is-illegal S D) NIL)	

			;If the move is legal, then we can move the keeper
			(t 
				(move-keeper S D r c)
			)
		)
	)

)

; Update the old state of the keeper

(defun move-keeper (S D r c)
	(let* ((updated_state (execute-move S D r c))) 
		(cond

			;Check to see if the keeper is on a star
			; If so -> move the keeper and set to star
			((isKeeperStar (get-square S r c))
				(set-square updated_state r c star)
			)

			;Else it is not a star and should be set blank
			(t
				(set-square updated_state r c blank )
			)
		)
	)
)

; Execute the actual keeper move
(defun execute-move (S D r c)
	(let* ((obj_up (get-square S (- r 1) c))
		    (obj_down (get-square S (+ r 1) c))
		    (obj_left (get-square S r (- c 1)))
		    (obj_right (get-square S r (+ c 1))))
		(cond
				;If we've gotten here, we know it's a legal move to push the keeper up. This means
				;that whatever is in the position 1 unit up from our D value can be pushed up 1 more
				;unit.
				;Set the keeper to be keeperStar if we identify move location has a star or boxStar

			((equal D 1) ;UP
				;Check if we find a star or a boxstar
				(cond
					((or
						(isStar obj_up)
						(isBoxStar obj_up)
					) (set-square (move-objects S D r c) (- r 1) c keeperstar))

					; This means that there is only a blank spot to move to
					(t 
					  (set-square (move-objects S D r c) (- r 1) c keeper)
					)
				)
			)

			((equal D 2) ;DOWN
				(cond
					((or
						(isStar obj_down)
						(isBoxStar obj_down)
					) (set-square (move-objects S D r c) (+ r 1) c keeperstar))

					(t
						(set-square (move-objects S D r c) (+ r 1) c keeper)
					)
				)
			)
			((equal D 3) ;LEFT
				(cond
					((or
						(isStar obj_left)
						(isBoxStar obj_left)
					) (set-square (move-objects S D r c) r (- c 1) keeperstar))

					(t
						(set-square (move-objects S D r c) r (- c 1) keeper)
					)
				)
			)
			((equal D 4) ;RIGHT
				(cond
					((or 
						(isStar obj_right)
						(isBoxStar obj_right)
					) (set-square (move-objects S D r c) r (+ c 1) keeperstar))

					(t
						(set-square (move-objects S D r c) r (+ c 1) keeper)
					)
				)
			)				
		)
	)

)

;Move objects
; -We check to see if there is an object in the direction we want to move in
; 	If yes -> move the object 1 more unit in that direction
; 	If no  -> return input state S (no mods)

;note: If we reach this function we know we have a valid move, and can move whatever box
; 		one unit in the direction we are requesting.
(defun move-objects (S D r c)
	(let* ((obj_up (get-square S (- r 1) c))
		    (obj_down (get-square S (+ r 1) c))
		    (obj_left (get-square S r (- c 1)))
		    (obj_right (get-square S r (+ c 1))))
		(cond
			((equal D 1) ;UP

				;If the object up is a box or box star and we have a legal move, we can move it
				;up one more unit
				(cond
					((or
						(isBox obj_up)
						(isBoxStar obj_up)
					) (cond
							((isStar (get-square S (- r 2) c)) 
								(set-square S (- r 2) c boxStar)
							)
							((isBlank (get-square S (- r 2) c))
								(set-square S (- r 2) c box)
							)
					   )
					)

					;If no box to move, then return an unmodified state S
					(t
						S
					)
				)
			)
			((equal D 2) ;DOWN
				(cond
					((or
						(isBox obj_down)
						(isBoxStar obj_down)
					)(cond
							((isStar (get-square S (+ r 2) c)) 
								(set-square S (+ r 2) c boxStar)
							)
							((isBlank (get-square S (+ r 2) c))
								(set-square S (+ r 2) c box)
							)
					   )
					 )
					;If no box to move, then return an unmodified state S
					(t
						S
					)					
				)
			)
			((equal D 3) ;LEFT
				(cond
					((or
						(isBox obj_left)
						(isBoxStar obj_left)
					) (cond
							((isStar (get-square S r (- c 2))) 
								(set-square S r (- c 2) boxStar)
							)
							((isBlank (get-square S r (- c 2)))
								(set-square S r (- c 2) box)
							)
					   )
					 )

					;If no box to move, then return an unmodified state S
					(t
						S
					)					
				)
			)
			((equal D 4) ;RIGHT
				(cond
					((or
						(isBox obj_right)
						(isBoxStar obj_right)
					) (cond
							((isStar (get-square S r (+ c 2))) 
								(set-square S r (+ c 2) boxStar)
							)
							((isBlank (get-square S r (+ c 2)))
								(set-square S r (+ c 2) box)
							)
					   )
					 )


					;If no box to move, then return an unmodified state S
					(t
						S
					)

				)
			)	
		 )
	)
)


; IS-ILLEGAL
; Check to see if moving direction D from r c in state S is illegal
; If it's a wall -> illegal
; If it's a box and 
(defun is-illegal (S D)
	(let* ((pos (getKeeperPosition s 0))
		 (c (car pos))
		 (r (cadr pos)))
	 
	 	(cond
	 		;UP
	 		((equal D 1)
	 			 (let* ((up_val (get-square S (- r 1) c)) 
	 			 		(up_val2 (get-square S (- r 2) c)) )
	 			 	(cond
	 			 	;	((or (equal up_val NIL) (equal up_val2 NIL)) T)

	 			 		((equal up_val NIL) T)
	 			 		((isWall up_val) t)
	 			 		((or (isBox up_val) (isBoxStar up_val))
	 			 			(cond
	 			 				((equal up_val2 NIL) T)
	 			 				(t
	 			 					(or
			 			 				;Fail if 2 positions up is a wall, box+goal, or box
			 			 				(isBox up_val2)
			 			 				(isWall up_val2)
			 			 				(isBoxStar up_val2)
			 			 			)
	 			 				)
	 			 			)
	 			 			
	 			 		)

	 			 	)
	 			 )
	 		)

	 		;DOWN
	 		((equal D 2)
	 			 (let* ((down_val (get-square S (+ r 1) c)) 
	 			 		(down_val2 (get-square S (+ r 2) c)) )
	 			 	(cond
;	 			 		((or (equal down_val NIL) (equal down_val2 NIL)) T)
						((equal down_val NIL) T)

	 			 		((isWall down_val) t)
	 			 		((or (isBox down_val) (isBoxStar down_val))
	 			 			(cond
	 			 				((equal down_val2 NIL) T)
	 			 				(t
	 			 					(or
			 			 				;Fail if 2 positions up is a wall, box+goal, or box
			 			 				(isBox down_val2)
			 			 				(isWall down_val2)
			 			 				(isBoxStar down_val2)
			 			 			)
	 			 				)
	 			 			)
	 			 			
	 			 		)
	 			 	)
	 			 )

	 		)

	 		;LEFT
	 		((equal D 3)
	 			 (let* ((left_val (get-square S r (- c 1))) 
	 			 		(left_val2 (get-square S r (- c 2))) )
	 			 	(cond
	 			 	;	((or (equal left_val NIL) (equal left_val2 NIL)) T)
	 			 		((equal left_val NIL) T)

	 			 		((isWall left_val) t)
	 			 		((or (isBox left_val) (isBoxStar left_val))
	 			 			(cond
	 			 				((equal left_val2 NIL) T)
	 			 				(t
	 			 					(or
			 			 				;Fail if 2 positions up is a wall, box+goal, or box
			 			 				(isBox left_val2)
			 			 				(isWall left_val2)
			 			 				(isBoxStar left_val2)
			 			 			)
	 			 				)
	 			 			)
	 			 			
	 			 		)
	 			 		(t NIL)
	 			 	)
	 			 )
	 		)

	 		;RIGHT
	 		((equal D 4)
	 			 (let* ((right_val (get-square S r (+ c 1))) 
	 			 		(right_val2 (get-square S r (+ c 2))) )
	 			 	(cond
	 			 		;((or (equal right_val NIL) (equal right_val2 NIL)) T)
	 			 		((equal right_val NIL) T)

	 			 		((isWall right_val) t)
	 			 		((or (isBox right_val) (isBoxStar right_val))
	 			 			(cond
	 			 				((equal right_val2 NIL) T)
	 			 				(t
	 			 					(or
			 			 				;Fail if 2 positions up is a wall, box+goal, or box
			 			 				(isBox right_val2)
			 			 				(isWall right_val2)
			 			 				(isBoxStar right_val2)
			 			 			)
	 			 				)
	 			 			)
	 			 			
	 			 		)

	 			 	)
	 			 )
	 		)

	 	)
 	)
)


; GETSQUARE
; We want to get the number that is stored at position (r,c) on the grid.
; Inputs: state S, row R, col C
; Outputs: 
;			Outside of the grid -> 1 
;			Valid position -> atom value (integer)

(defun get-square (S R C)
	(cond
		;If the row value is zero, we want to look for the column in here
		((equal R 0) (get-val (first S) C))

		;The row value is > 0, so we want to go to the next option in the list
		((> R 0) (get-square (rest S) (- R 1) C))

	)
)

; Return a value from the list at index "i", indexed from 0.
; Input: List of integers (L), index (i)
; Output: Number at pos i
(defun get-val (L i)
	(cond
		((> i (- (length L) 1)) 1)	;return 1 if it is out of bounds
		((< i 0) NIL)
		((equal i 0) (first L))
		(t (get-val (rest L) (- i 1)))
	)
)

; Input: 
;	S: state
;	R: row
;	C: col
;	V: value to set
; Output: State resulting from value(r, c) = v
(defun set-square (S R C V)
	(cond
		;If R=0, we want to replace a value in the first of S and return
		((equal R 0) 
			(cond
				((equal (set-val (first S) C V) NIL) NIL)
				(t (append (list(set-val (first S) C V)) (rest S)))
			)
		)

		
		;If R > 0 then we need to go to the next row
		((> R 0) 
			(cond
				((equal (set-square (rest S) (- R 1) C V) NIL) NIL)
				(t (append (list(first S)) (set-square (rest S) (- R 1) C V)))
			)
		)
	)
)

; Replace a value in the list L at index i with value v.
(defun set-val (L i v)
	(cond
		;We check to see if we're trying to change something out of bounds, and return NIL
		((> i (- (length L) 1)) NIL)

		;We replace the front and return the rest of the list
		((equal i 0) (append (list v) (rest L)))
		(t (append (list(first L)) (set-val (rest L) (- i 1) v)))
	)
)
;------------------------------------------------------------------------------------------



; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s)
	0
  )

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.

; Boxes not on goals are represented by integer "2", so we only need to count 
; the number of "2" occurrences in the state.

; h1 is admissible. It will not overestimate the cost to reach a goal state, because
; the cost to reach a goal state is at minimum the #of moves to move the box to the goal
(defun h1 (s)
	(cond

		;If it's an atom, we check to see if it's a 2 or not.
		; 	If 2 -> return 1 to add to the count
		;	If != 2 -> return 0 to not affect the count
		((atom s) 
			(cond
				((equal s 2) 1)
				(t 0)
			)
		)

		;If s is a list, we recursively split it up and add the partitions.
		;	sum((2s in first) + (2s in rest of list))
		((listp s)
			(+ (h1 (first s)) (h1 (rest s)) )
		)
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;


;Experimental data results for h804258351:
; p2: expanded 62, generated 180
; p10: expanded 6587, generated 17241
; p12: expanded 7098, generated 22301
; p13: expanded 1205, generated 4061
; p14: expanded 3845, generated 10351
; p15: expanded 13163, generated 13163
; p16: expanded: 155067, generated 398908
; p17: expanded 62368, generated 177050
; p18: expanded 303, generated 1024
; p19: expanded 81, generated 245
; p20-p22: stack overflow
; p21: 

(defun h804258351 (s)
  (let* ((pos (getKeeperPosition s 0))
		 (c (car pos))
		 (r (cadr pos)))

		 (*
			 (get-move-cost 
			 	(append (list r) (list c))
			 	(get-box-coords S 0)
			 )
			 (h1 s)
		 )


  )
)

;We need to sum the Manhattan distances from player to each box
; player position is list pos = (r c)
(defun get-move-cost (pos box_coords )
	(cond
		((null box_coords) 0)
		(t
			(+
				(calc-manhattan pos (first box_coords))
				(get-move-cost pos (rest box_coords))
			)
		)

	)
)

;Calculate Manhattan Distance between two points (x1, y1) (x2, y2)
(defun calc-manhattan (P1 P2)
	(+ 
		(abs (- (first P1) (first P2)))
		(abs (- (second P1) (second P2)))
	)
)

;Searches row for box, returns list of col positions L_c
(defun search-row (R i)
	(cond
		((null R) NIL)
		((isBox (first R)) (append (list i) (search-row (rest R) (+ i 1)) ))
		(t
			(search-row (rest R) (+ i 1))
		)
	)	
)

;Create coordinate list
(defun create-pairs (R_num C_boxvals)
	(cond
		((null C_boxvals) NIL)
		(t
			(append
				(list(append (list R_num) (list(first C_boxvals))))
				(create-pairs R_num (rest C_boxvals))
			)
		)
	)

)


;Get the coordinates of all boxes
(defun get-box-coords (S i)
	(cond
		((null S) NIL)

		;row 0 from this perspective
		(t
			(append
				(create-pairs i (search-row (first S) 0))
				(get-box-coords (rest S) (+ i 1))
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
