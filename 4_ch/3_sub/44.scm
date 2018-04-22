;;This solution inspired by some solutions available online.
;;Initial solution was recursive rather than iterative, generating a solution-board in whole
;;through amb, and then checked the entire thing for safety.  (Didn't work because I used amb rather
;;than list-amb, as I eventually narrowed my failures down to while trying out the 
;;online-inspired attempts.


(define (require p) (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
		((null? (cdr items)) true)
		((member (car items) (cdr items)) false)
		(else (distinct? (cdr items)))))

(define (safe? sol)
  (display sol)
  (define (iter new old d)
	(cond ((null? old) true)
		  ((= new (car old)) false)
		  ((= new (+ (car old) d)) false)
		  ((= new (- (car old) d)) false)
		  (else (iter new (cdr old) (+ d 1)))))
  (iter (car sol) (cdr sol) 1))

(define (integers-between n m)
  (if (> n m)
	'()
	(cons n (integers-between (+ n 1) m))))

(define (list-amb li)
(if (null? li)
(amb)
(amb (car li) (list-amb (cdr li)))))


(define (queens n)
  (define pos (integers-between 1 n))
  (define (queen-cols sol k)
	(if (= k 0) 
	  sol
	  (let ((x (amb pos)))
		(let ((new (cons (list-amb pos) sol)))
		  (require (safe? new))
		  (queen-cols new (- k 1))))))
  (queen-cols '() n))
