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


(define (cycle? x)
  (let ((previously-encountered-cdrs '()))
	(define (cuddering current)
	  (cond ((null? (cdr current)) #f)
			((memq (cdr current) previously-encountered-cdrs) #t)
			(else
			  (set! previously-encountered-cdrs (cons (cdr current) previously-encountered-cdrs))
			  (cuddering (cdr current)))))
	(cuddering x)))

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(cycle? (make-cycle (list 'a 'b 'c)))
(cycle? (list 1 2 3 45 90 09493 81))
