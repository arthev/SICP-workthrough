(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
		((= x (entry set)) true)
		((< x (entry set))
		 (element-of-set? x (left-branch set)))
		((> x (entry set))
		 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
		((= x (entry set)) set)
		((< x (entry set))
		 (make-tree (entry set)
					(adjoin-set x (left-branch set))
					(right-branch set)))
		((> x (entry set))
		 (make-tree (entry set)
					(left-branch set)
					(adjoin-set x (right-branch set))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
	(if (null? tree)
	  result-list
	  (copy-to-list (left-branch tree)
					(cons (entry tree)
						  (copy-to-list
							(right-branch tree)
							result-list)))))
  (copy-to-list tree '()))

(define a (make-tree 7 (make-tree 3 (make-tree 1 '() '())
								    (make-tree 5 '() '()))
					   (make-tree 9 '() (make-tree 11 '() '()))))

(define b (make-tree 3 (make-tree 1 '() '())
					   (make-tree 7 (make-tree 5 '() '())
								    (make-tree 9 '() (make-tree 11 '() '())))))

(define c (make-tree 5 (make-tree 3 (make-tree 1 '() '())
						  		    '())
					   (make-tree 9 (make-tree 7 '() '())
								    (make-tree 11 '() '()))))


;2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
	(cons '() elts)
	(let ((left-size (quotient (- n 1) 2)))
	  (let ((left-result (partial-tree elts left-size)))
		(let ((left-tree (car left-result))
			  (non-left-elts (cdr left-result))
			  (right-size (- n (+ left-size 1))))
		  (let ((this-entry (car non-left-elts))
				(right-result (partial-tree (cdr non-left-elts) right-size)))
			(let ((right-tree (car right-result))
				  (remaining-elts (cdr right-result)))
			  (cons (make-tree this-entry
							   left-tree
							   right-tree)
					remaining-elts))))))))

;partial-tree works by dividing its workload in two per step. The elements to the left of
;((length elts)/2)th elt get turned into the left-branch next-partial-tree, whereas the elt
;point to by ((length elts)/2) becomes the entry node, and the rest get turned into the right-branch.
;Keeping track of unused elts is important for this splitting process. This is, of course,
;a recursive strategy.


;Tree drawn for (list->tree (list 1 3 5 7 9 11)):
;(make-tree 5 (make-tree 1 '() 3)
;             (make-tree 9 (make-tree 7 '() '())
;                          (make-tree 11 '() '())))

;Like tree->list-2, the function visits each node once, and combines them by cons, which runs in constant time,
;and is therefore theta(n).
