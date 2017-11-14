(define tolerance 0.0001)

(define (average a b)
  (/ (+ a b) 2))

(define (log2 x)
  (/ (log x) (log 2)))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter g n)
	(if (= n 1) g
	  (iter (compose f g) (-1+ n))))
  (iter f n))

(define (average-damp f)
  (lambda (x) (average x (f x))))


(define (nthroot x n)
  (fixed-point ((repeated average-damp (floor (log2 n)))
				  (lambda (y) (/ x (expt y (-1+ n)))))
			   1.0))



(nthroot 10000 80)
