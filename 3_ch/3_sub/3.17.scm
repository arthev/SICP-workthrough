;(define (count-pairs x)
;  (if (not (pair? x))
;	0
;	(+ (count-pairs (car x))
;	   (count-pairs (cdr x))
;	   1)))
(define (count-pairs x)
  (let ((counted-list '()))
	(define (counter x)
	  (cond ((not (pair? x)) 0)
			((memq x counted-list) 0)
			(else
			  (set! counted-list (cons x counted-list))
			  (+ (counter (car x))
				 (counter (cdr x))
				 1))))
	(counter x)))


(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


;Should return three
(count-pairs (cons (cons 1 '()) (cons 3 '())))

;Should return four
(define a (cons 'a 'b))
(define b (cons 'c a))
(define x (cons b a))
(count-pairs x)

;Should return seven
(define a (cons 'a 'b))
(define b (cons a a))
(define x (cons b b))
(count-pairs x)

;Won't ever termine
(count-pairs (make-cycle (list 'a 'b 'c)))
