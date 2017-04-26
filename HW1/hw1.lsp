
; PAD takes in an input N 
; Returns Nth Padovan number
; with larger inputs, it takes time due to recursion
(defun PAD (N) 
	(cond ((= n 0) 1)
		  ((= n 1) 1)
		  ((= n 2) 1) ; base cases
		  (T (+ (PAD (- n 2)) (PAD (- n 3)))) ; recursive case based on definition
	)
)

; SUMS takes in an input N 
; Returns number of + operations required to compute (PAD N)
(defun SUMS (N)
	(cond ((= n 0) 0)
		  ((= n 1) 0)
		  ((= n 2) 0) ;base cases, no addition when we have a trivial N
		  (T (+ (SUMS (- n 2)) (SUMS (- n 3)) 1)) ;recursive case
		  ; one extra addition plus the ones required for the sub cases
	)
)

; ANON takes in an argument that could be a list or an atom
; Replaces all the data stored in the list with '?'
(defun ANON (L)
	(cond ((null L) '()) ; return empty list if it is an empty list
		  ((atom L) '?)	; else if its an atom, return a ?
		  (T (cons (ANON (car L)) (ANON (cdr L)))) 
		  ; call ANON on first element, and rest of L, and combine
	)
)