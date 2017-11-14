;first try
;(define (subsets s)
;  (if (null? s)
;	(list '())
;	(let ((rest (subsets (cdr s))))
;	  (append rest (map (lambda (x) (list (car s) x)) rest)))))

(define (subsets s)
  (if (null? s)
	(list s)
	(let ((rest (subsets (cdr s))))
	  (append rest (map (lambda (x) (cons (car s) x))
						rest)))))

(subsets (list 1 2 3))

;The function works because at any point in the recursion,
;rest is the elements cdr s, which can then be gone over and get
;car s inserted before them in the subset. This goes down
;to the empty set, which gets the permutations with one number in
;front of them, and then those generate the two-number lists, and so on,
;building up the permutations from the base case empty list to lists of more and more elements.
