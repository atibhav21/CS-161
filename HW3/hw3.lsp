;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
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
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
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

; checks if a row has an incomplete goal within the row
; takes a row as its input
; returns true if row does not violate goal state and nil otherwise
(defun check-row-for-completeness (r)
	(cond
		((null r) T)
		((isBox (first r)) nil)
		(T (check-row-for-completeness (rest r)))
	)
)

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond 
  	((null S) T)
  	((check-row-for-completeness (first s)) (goal-test (rest s)))
  	(T nil)
  )
);end defun

; returns the integer value at the specified row and column value
; indexed as: top left corner is (0,0)
(defun get-square (s r c)
	(cond
		((null S) wall)
		((> r 0) (get-square (rest s) (- r 1) c)) ; move to next row
		((= r 0) ; row found
			(cond 
				((= c 0) (first (first s))) ; found the element! 
				((> c 0) (get-square(cons (rest (first s)) (rest s)) r (- c 1))) ; move to next column in same row
				(T wall) ; out of scope value
			)
		)
		(T wall) ; out of scope value
	)
)

; Changes the item at (r,c) to v
; Indexed as: top left corner is (0,0)
; returns updated state if valid row and column index, else returns nil
(defun set-square(s r c v)
	(cond 
		((null s) nil)
		((> r 0) (cons (first s) (set-square (rest s) (- r 1) c v)))
		((= r 0)
			(cond
				((> c 0) (cons (cons (first (first s)) (first (set-square (cons (rest (first s)) (rest s)) r (- c 1) v)) ) (rest s)))
				((= c 0) (cons (cons v (rest (first s))) (rest s)))
				(T nil)
			)
		)
		(T nil) ; should never return nil!!!
	)
)

; update the new position of the keeper
(defun current-keeper-move (s kp_row kp_col)
	(let ((currentStatus (get-square s kp_row kp_col)))
		(cond 
			((isBlank currentStatus) keeper)
			((isStar currentStatus) keeperstar)
			(T nil)
		)
	)
)

; update the current position of the keeper after moving the keeper
(defun after-keeper-move (s kp_row kp_col)
	(let ((currentStatus (get-square s kp_row kp_col)))
		(cond
			((isKeeper currentStatus) blank)
			((isKeeperStar currentStatus) star)
			(T nil)
		)
	)
)

; update the new position of the box
(defun current-box-move (s r c)
	(let ((currentStatus (get-square s r c)))
		(cond
			((isBlank currentStatus) box)
			((isStar currentStatus) boxstar)
			(T nil)
		)
	)
)

; update the current position of the box after moving the box
(defun after-box-move (s r c)
	(let ((currentStatus (get-square s r c)))
		(cond
			((isBox currentStatus) blank)
			((isBoxStar currentStatus) star)
			(T nil)
		)
	)
)

; move in the left direction
(defun move-left (s kp_row kp_col last_row last_col)
	(cond 
		((isBlank (get-square s kp_row (- kp_col 1))) 
			; direction to move in is blank, so update player, box and position next to box
			(set-square (set-square s kp_row kp_col (after-keeper-move s kp_row kp_col)) kp_row (- kp_col 1) keeper)
		)
		((isStar (get-square s kp_row (- kp_col 1)))
			; direction to move in has a star so update that to contain star and player
			(set-square (set-square s kp_row kp_col (after-keeper-move s kp_row kp_col)) kp_row (- kp_col 1) keeperstar)
		)
		((and   (>= (- kp_col 2) 0)
				(or (isBox  (get-square s kp_row (- kp_col 1))) (isBoxStar (get-square s kp_row (- kp_col 1)))) ; is a box
				(or (isBlank (get-square s kp_row (- kp_col 2))) (isStar (get-square s kp_row (- kp_col 2)))))
					; square is either blank or a goal state
					; direction to move in has a box next to player, so update player, box and position next to box
			(let ((moveBoxState (set-square s kp_row (- kp_col 2) (current-box-move s kp_row (- kp_col 2)))))
				(let ((updatedBoxState (set-square moveBoxState kp_row (- kp_col 1) (after-box-move moveBoxState kp_row (- kp_col 1)))))
					(let ((movePlayerState (set-square updatedBoxState kp_row (- kp_col 1) (current-keeper-move updatedBoxState kp_row (- kp_col 1)))))
						(set-square movePlayerState kp_row kp_col (after-keeper-move movePlayerState kp_row kp_col))
					)
				)
			)
		)			
		(T nil)
	)

)

(defun move-right (s kp_row kp_col last_row last_col)
	(cond 
		((isBlank (get-square s kp_row (+ kp_col 1))) 
			; direction to move in is blank, so update player, box and position next to box
			(set-square (set-square s kp_row kp_col (after-keeper-move s kp_row kp_col)) kp_row (+ kp_col 1) keeper)
		)

		((isStar (get-square s kp_row (+ kp_col 1)))
			; direction to move in has a star so update that to contain star and player
			(set-square (set-square s kp_row kp_col (after-keeper-move s kp_row kp_col)) kp_row (+ kp_col 1) keeperstar)
		)
		((and   (<= (+ kp_col 2) last_col)
				(or (isBox  (get-square s kp_row (+ kp_col 1))) (isBoxStar (get-square s kp_row (+ kp_col 1)))) ; is a box or a box and goal
				(or (isBlank (get-square s kp_row (+ kp_col 2))) (isStar (get-square s kp_row (+ kp_col 2)))) ) ; blank state or goal state to move the box to

			;(set-square (set-square (set-square s kp_row kp_col blank) kp_row (+ kp_col 1) keeper) kp_row (+ kp_col 2) box)
			; move the box and update the new position of box correctly
			(let ((moveBoxState (set-square s kp_row (+ kp_col 2) (current-box-move s kp_row (+ kp_col 2))))) 
				; use above state to modify the old position of the box correctly
				(let ((updatedBoxState (set-square moveBoxState kp_row (+ kp_col 1) (after-box-move moveBoxState kp_row (+ kp_col 1))))) 
					; move player into the space vacated by moving the box
					(let ((movePlayerState (set-square updatedBoxState kp_row (+ kp_col 1) (current-keeper-move updatedBoxState kp_row (+ kp_col 1)))))
						; update the old position of the player
						(set-square movePlayerState kp_row kp_col (after-keeper-move movePlayerState kp_row kp_col))
					)
				)
			)
		)			
		(T nil)
	)
)

(defun move-up (s kp_row kp_col last_row last_col)
	(cond
		((isBlank (get-square s (- kp_row 1) kp_col))
			(set-square (set-square s kp_row kp_col (after-keeper-move s kp_row kp_col)) (- kp_row 1) kp_col keeper)
		)
		((isStar (get-square s (- kp_row 1) kp_col))
			(set-square (set-square s kp_row kp_col (after-keeper-move s kp_row kp_col)) (- kp_row 1) kp_col keeperstar)
		)
		((and (>= (- kp_row 2) 0)
			  (or (isBox (get-square s (- kp_row 1) kp_col)) (isBoxStar (get-square s (- kp_row 1) kp_col)) )
			  (or (isBlank (get-square s (- kp_row 2) kp_col)) (isStar (get-square s (- kp_row 2) kp_col))))
				;(set-square (set-square (set-square s kp_row kp_col blank) (- kp_row 1) kp_col keeper) (- kp_row 2) kp_col box)

			(let ((moveBoxState (set-square s (- kp_row 2) kp_col (current-box-move s (- kp_row 2) kp_col ))))
				(let ((updatedBoxState (set-square moveBoxState (- kp_row 1) kp_col (after-box-move moveBoxState (- kp_row 1) kp_col ))))
					(let ((movePlayerState (set-square updatedBoxState (- kp_row 1) kp_col  (current-keeper-move updatedBoxState (- kp_row 1) kp_col ))))
						(set-square movePlayerState kp_row kp_col (after-keeper-move movePlayerState kp_row kp_col))
					)
				)
			)
		)
		(T nil)
	)
)

(defun move-down (s kp_row kp_col last_row last_col)
	(cond
		((isBlank (get-square s (+ kp_row 1) kp_col))
			(set-square (set-square s kp_row kp_col (after-keeper-move s kp_row kp_col)) (+ kp_row 1) kp_col keeper)
		)
		((isStar (get-square s (+ kp_row 1) kp_col))
			(set-square (set-square s kp_row kp_col (after-keeper-move s kp_row kp_col)) (+ kp_row 1) kp_col keeperstar)
		)
		((and (>= (+ kp_row 2) 0)
			  (or (isBox (get-square s (+ kp_row 1) kp_col)) (isBoxStar (get-square s (+ kp_row 1) kp_col)))
			  (or (isBlank (get-square s (+ kp_row 2) kp_col)) (isStar (get-square s (+ kp_row 2) kp_col))))
				;(set-square (set-square (set-square s kp_row kp_col blank) (+ kp_row 1) kp_col keeper) (+ kp_row 2) kp_col box)
			(let ((moveBoxState (set-square s (+ kp_row 2) kp_col (current-box-move s (+ kp_row 2) kp_col ))))
				(let ((updatedBoxState (set-square moveBoxState (+ kp_row 1) kp_col (after-box-move moveBoxState (+ kp_row 1) kp_col ))))
					(let ((movePlayerState (set-square updatedBoxState (+ kp_row 1) kp_col  (current-keeper-move updatedBoxState (+ kp_row 1) kp_col ))))
						(set-square movePlayerState kp_row kp_col (after-keeper-move movePlayerState kp_row kp_col))
					)
				)
			)
		)
		(T nil)
	)
)

; tries a move in the direction of dir
; dir can be 'L 'R 'U 'D
(defun try-move (s dir)
	(let ((keeperPosition (getKeeperPosition s 0)) (last_col (- (length (first s)) 1) ) (last_row (- (length s) 1)))
		(let ((kp_row (second keeperPosition)) (kp_col (first keeperPosition)))
			(cond
				((and (equal dir 'L) (> kp_col 0))
					(move-left s kp_row kp_col last_row last_col)
				)
				((and (equal dir 'R) (< kp_col last_col))
					(move-right s kp_row kp_col last_row last_col)
				)
				((and (equal dir 'U) (> kp_row 0))
					(move-up s kp_row kp_col last_row last_col)
				)
				((and (equal dir 'D) (< kp_row last_row))
					(move-down s kp_row kp_col last_row last_col)
				)
				(T nil)
			)
		)
	)
)

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
; 
;
(defun next-states (s)
	;(progn
	;  (printstate s)
	  (let* ((pos (getKeeperPosition s 0))
		 (x (car pos))
		 (y (cadr pos))
		 ;x and y are now the coordinate of the keeper in s.
		 (result (list (try-move s 'U) (try-move s 'R) (try-move s 'D) (try-move s 'L) ))
		 )
	    (cleanUpList result);end
	   );end let

	; )
 );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; Is this heuristic admissible?
; Yes, the heuristic is admissible because we need at least the "number of boxes misplaced" steps to reach the goal state
(defun h1 (s)
  (cond 
  		((null s) 0)
  		((null (first s)) (h1 (rest s))) ; no more left in current row so move to next row
  		((isBox (first (first s))) (+ 1 (h1 (cons (rest (first s)) (rest s))))) ; is box so add one
  		(T (h1 (cons (rest (first s)) (rest s)))) ; not just a box so move to next item in row
  )
)

; return the positions of the item v in the current row
; l is a list of lists, r is the current row, c is the current column
; tail recursive
(defun check-row-for-item (currentRow v r c l)
	(cond 
		((null currentRow) l)
		((= v (first currentRow)) (check-row-for-item (rest currentRow) v r (+ c 1) (append l (list (list r c)))))
		(T (check-row-for-item (rest currentRow) v r (+ c 1) l))
	)
)

; heuristic helper function
; return the positions of the items
; tail recursive function
(defun findItemPositions (s v currentRow currentList)
	(cond
		((null s) currentList)
		(T (findItemPositions (rest s) v (+ currentRow 1) (append currentList (check-row-for-item (first s) v currentRow 0 nil))))
	)
)

; returns manhattan distance between position 1 and position 2, both of the format (r c)
(defun computeDistance (pos1 pos2)
	(+ (abs (- (first pos1) (first pos2))) (abs (- (second pos1) (second pos2))))
)

; returns the smallest manhattan distance between a box and a goal state (not including boxes already in goal states)
; box is position of the box in the format (r c)
; goalList is the positions of the all the goals, of the format ((r c))
; tail recursive
(defun getManhattanDistance (box goalList currentMin)
	(cond 
		((null goalList) currentMin)
		(T	(let ((distance (computeDistance box (first goalList))))
				(cond 
					((< distance currentMin) (getManhattanDistance box (rest goalList) distance))
					(T (getManhattanDistance box (rest goalList) currentMin))
				)
			)
		)
	)	
)

(defun heuristicHelper (boxPositions goalPositions currentCost)
	(cond 
		((null boxPositions) currentCost)
		((null goalPositions) currentCost)
		(T (let ((firstDistance (computeDistance (first boxPositions) (first goalPositions))))
				(heuristicHelper (rest boxPositions) goalPositions (+ currentCost (getManhattanDistance (first boxPositions) (rest goalPositions) firstDistance)))
			)
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
(defun h804598987 (s)
	(let ((boxPositions (findItemPositions s box 0 nil)) (goalPositions (findItemPositions s star 0 nil)))
 		(heuristicHelper boxPositions goalPositions 0)
 	)
 	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
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
