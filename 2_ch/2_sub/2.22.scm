(define square (lambda (x) (* x x)))
(define nil '())
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

;(define (square-list items)
;  (define (iter things answer)
;	(if (null? things)
;	  answer
;	  (iter (cdr things)
;			(cons (square (car things))
;				  answer))))
;  (iter items nil))

;The above square-list procedure doesn't work,
;because it builds the list in the reverse order.
;Answer starts out as '(), then becomes (1 '()),
;then (4 1 '()), because each new term is consed
;in front of the rest.

;(define (square-list items)
;  (define (iter things answer)
;	(if (null? things)
;	  answer
;	  (iter (cdr things)
;			(cons answer
;				  (square (car things))))))
;  (iter items nil))

;The above square-list procedure doesn't work,
;because now there isn't even a list anymore!
;It's a hierarchy of pairs, certainly, but
;now the empty list is the first element, and the
;subsequent elements are consed behind it.

(square-list (list 1 2 3 4 5 6 7 8 9 10 11))
