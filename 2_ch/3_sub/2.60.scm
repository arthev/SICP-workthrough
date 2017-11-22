;Library
(define (element-of-set? x set)
  (cond ((null? set) false)
		((equal? x (car set)) true)
		(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
		 (cons (car set1) (intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (if (null? set1)
	set2
	(union-set (cdr set1) (adjoin-set (car set1) set2))))

(union-set '(a b c g h 1 4) '(b c d 2 h n))


;Because of the allowed duplicates, the efficiency of this approach
;is lower than the previous... But primarily because of constant values.
;The adjoin-set is now theta(1). Union-set has theta(n), for the size of set1.
;Intersection-set has theta(n^2) and element-of-set? theta(n) like before.
;The space requirements of each set are now larger, though.
;This representation would be useful if space wasn't an issue,
;and the majority of operations were unions, and possibly look-ups of values
;that were recently added to the set. For the majority of applications, the
;cleaner non-duplicate representation is probably better, though.
