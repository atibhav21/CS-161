;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

;(BFS (append (car FRINGE) (cdr FRINGE)))

;(append (BFS (cdr FRINGE)) (BFS (car FRINGE)))

; TODO: comment code
(defun BFS (FRINGE)
    (cond ((null FRINGE) NIL)
          ((listp (first FRINGE)) (BFS (append (rest FRINGE) (first FRINGE))) ) ; node has children so add the first element to the end of the cdr
          ; modifies FRINGE so that it finds the elements at the current level and creates a modified tree for the next level
          (T (cons (first FRINGE) (BFS (rest FRINGE)))) ; just an element in the list
    )
        
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (cond   
        ((equal S '(T T T T)) T) ; (T T T T) is final state so return true if S is equal to this
        (T NIL) ; false otherwise
    )
)


; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
    (cond
        ((equal A 'h) ; move homer only
            (cond
                
                ((or (equal (second S) (third S)) (equal (second S) (third S))) NIL) ; baby and poison/dog will be left on same sid so return NIL
                
                (T (list (cons (NOT (first S)) (rest S)))) ; valid move so move Homer only
            )
        ) 
        ((equal A 'b) ; try to move homer and the baby
            (cond 
                ((equal (first S) (second S)) ; homer on the same side as baby, so flip both. can always move this way
                    (list (list (NOT (first S)) (NOT (second S)) (third S) (fourth S))))
                (T NIL); invalid state as homer is not on the same side as baby
            )
        ) 
        ((equal A 'd) ; try to move homer and dog
            (cond ((not (equal (first S) (third S))) NIL) ; homer not on same side as dog
                  ((equal (second S) (fourth S)) NIL) ; move will result in baby alone on same side as poison
                  (T (list (list (NOT (first S)) (second S) (NOT (third S)) (fourth S)))) ; valid move
            )
        ) 
        ((equal A 'p) ; move homer and poison
            (cond ((not (equal (first S) (fourth S))) NIL) ; homer not on same side as poison
                  ((equal (second S) (third S)) NIL) ; move will result in dog and baby on same side
                  (T (list (list (NOT (first S)) (second S) (third S) (NOT (fourth S))))) ; valid move
            )
        ) 
        (T NIL); invalid argument passed in as A
    )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    (append (NEXT-STATE S 'b) (NEXT-STATE S 'h) (NEXT-STATE S 'd) (NEXT-STATE S 'p) )
    ; find each of the possible next states and append them to form a list of possible states
    ; is a list of lists, since the each state itself is a list
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond 
        ((null STATES) NIL) ; no more states left to compare with
        ((equal S (first STATES)) T) ; same state found
        (T (ON-PATH S (rest STATES))) ; not the same state, so check the rest of STATES
    )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (cond
        ((null STATES) NIL) ; no more states to explore
        ((FINAL-STATE (first STATES)) (append PATH (list (first STATES)))) ; reached final state so return the path used to reach it
        ((ON-PATH (first STATES) PATH) (MULT-DFS (rest STATES) PATH)) ; already checked this state before so skip this
        (T 
            (let ((getPath (DFS (first STATES) PATH ) ))
                (cond 
                    ((null getPath) (MULT-DFS (rest STATES) PATH))
                    (T getPath)
                )
            )
        ) ; go deeper in the search tree
    )
)


; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun  DFS (S PATH)
    (cond
        ((FINAL-STATE S) PATH)
        (T (MULT-DFS (SUCC-FN S) (append PATH (list S))))
    )
)
