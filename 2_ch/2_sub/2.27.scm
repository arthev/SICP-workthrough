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

(define (deep-reverse items)
  (define (looper items newlist)
	(cond ((null? items) newlist)
		  ((pair? (car items)) (looper (cdr items) 
							   (cons (deep-reverse (car items))
									 newlist)))
		  (else (looper (cdr items)
						(cons (car items)
							  newlist)))))
  (looper items '()))

(define x (list (list 1 2) (list 3 4)))
x
(reverse x)
(deep-reverse x)
