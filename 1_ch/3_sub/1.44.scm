(define dx 0.0001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter g n)
	(if (= n 1) g
	  (iter (compose f g) (- n 1))))
  (iter f n))

(define (smooth f)
  (lambda (x) 
	(/ (+ (f (+ x 0.0))
		  (f (+ x dx))
		  (f (- x dx)))
	   3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))


