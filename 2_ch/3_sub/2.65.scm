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

;2.65

(define (union-set-tree tree1 tree2)
  (list->tree (union-set (tree->list tree1)
						 (tree->list tree2)))
(define (intersection-set-tree tree1 tree2)
  (list->tree (intersection-set (tree->list tree1)
								(tree->list tree2))))

;These use the theta(n) implementations from the previous sub-sub-subchapter of the book.
;So, we have a chaining of functions. tree->list is theta(n).
;union/intersection-set are theta(n). list->tree is theta(n).
;Since constants aren't relevant to the theta notation,
;this becomes theta(n) + theta(n) + 2*theta(n) = 4*theta(n) = theta(n).
;Bam!
;For easy looking, here's the two functions from exercise 2.62 or so.

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
	'()
	(let ((x1 (car set1))
		  (x2 (car set2)))
	  (cond ((= x1 x2)
			 (cons x1 (intersection-set (cdr set1) (cdr set2))))
			((< x1 x2)
			 (intersection-set (cdr set1) set2))
			((< x2 x1)
			 (intersection-set set1 (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		(else
		  (let ((x1 (car set1))
				(x2 (car set2)))
			(cond ((= x1 x2)
				   (cons x1 (union-set (cdr set1) (cdr set2))))
				  ((< x1 x2)
				   (cons x1 (union-set (cdr set1) set2)))
				  ((< x2 x1)
				   (cons x2 (union-set set1 (cdr set2)))))))))














