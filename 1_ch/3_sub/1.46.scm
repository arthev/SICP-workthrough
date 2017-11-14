(define tolerance 0.0001)

;(define (iterative-improvement good-enough? improve)
;  (lambda (initial)
;	(let ((func ((lambda (x) (x x))
;			 (lambda (iter)
;				   (lambda (guess)
;					 (let ((next (improve guess)))
;					   (if (good-enough? guess next)
;						 next
;						 ((iter iter) next))))))))
;	  (func initial))))

(define (iterative-improvement good-enough? improve)
  (define (iter-imp guess)
	(if (good-enough? guess)
	  guess
	  (iter-imp (improve guess))))
  iter-imp)
							   
(define (sqrt x)
  ((iterative-improvement
	 (lambda (g) (< (abs (- (* g g) x)) 0.001))
	 (lambda (g) (/ (+ g (/ x g)) 2)))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improvement
	 (lambda (g) (< (abs (- g (f g))) tolerance))
	 f) first-guess))


(sqrt 9)
(fixed-point cos 1.0)
