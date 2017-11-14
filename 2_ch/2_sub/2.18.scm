(define (last-pair items)
  (if (null? (cdr items))
	items
	(last-pair (cdr items))))

(define (reverse items)
  (define (looper items newlist)
	(if (null? items)
	  newlist
	  (looper (cdr items) (cons (car items) newlist))))
  (looper items '()))

(reverse (list 1 4 9 16 25 36))
