(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;Old one for generic sets
(define (element-of-set? x set)
  (cond ((null? set) false)
		((= x (entry set)) true)
		((< x (entry set))
		 (element-of-set? x (left-branch set)))
		((> x (entry set))
		 (element-of-set? x (right-branch set)))))

;For the sets of records, which is... not a big difference!
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
		((equal? given-key (key (entry set-of-records)))
		 (entry set-of-records))
		((smaller? given-key (key (entry set-of-records)))
		 (lookup given-key (left-branch set-of-records)))
		((bigger? given-key (key (entry set-of-records)))
		 (lookup given-key (right-branch set-of-records)))))
