(define (same-parity x . l)
  (define (looper l)
	(cond ((null? l) '())
		  ((= (remainder x 2) (remainder (car l) 2)) (cons (car l) (looper (cdr l))))
		  (else (looper (cdr l)))))
  (cons x (looper l)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 8)

