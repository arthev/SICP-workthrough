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

(define (tree->list-1 tree)
  (if (null? tree)
	'()
	(append (tree->list-1 (left-branch tree))
			(cons (entry tree)
				  (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
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


(tree->list-1 a)
(tree->list-2 a)

(tree->list-1 b)
(tree->list-2 b)

(tree->list-1 c)
(tree->list-2 c)



;Both procedures seem to generate the same trees given the same input... tree->list-1 works from left to right, recursively,
;whereas tree->list-2 works from right to left, also recursively. (Not all variables necessary to pick up is available at any point,
;after all.)
;
;Both generate (1 3 5 7 9 11) for the tree trees.
;
;tree->list-2 uses cons as its main "combiner", which is constant, so traversing each leaf and combining in constant time gives O(n).
;tree->list-1 uses append, which goes over all the elements of the list it's about to append, and since the amount of elements in
;that list is halved for each step, the combiner action contributes a logn, while the base traversal contributes a factor of n.
;Thus O(nlogn)



