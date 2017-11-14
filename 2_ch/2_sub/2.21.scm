(define (map f items)
  (if (null? items)
	'()
	(cons (f (car items))
		  (map f (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
	   items))

;(define (square-list items)
;  (if (null? items)
;	'()
;	(cons (* (car items) (car items))
;		  (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x))
	   items))

(square-list (list 1 2 3 4 5 6 7 8 9 10 11))
