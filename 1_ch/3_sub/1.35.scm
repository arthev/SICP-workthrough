(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) 
	   tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)



;By definition, a fixed point of f is the value x so that f(x) = x.
;phi = (1 + sqrt(5))/2.
;Show that phi is a fixed point of x -> 1 + 1/x.
;phi = 1 + 1/phi
;phi = 1 + 2/(1+sqrt(5))
;(1+sqrt(5)) = 2 + 4/(1+sqrt(5))
;1 + 2*sqrt(5) + 5 = 2 + 2*sqrt(5) + 4
;6 = 6
;We see that the LHS and RHS can be manipulated into taking on the same values,
;therefore f(phi) = phi.
