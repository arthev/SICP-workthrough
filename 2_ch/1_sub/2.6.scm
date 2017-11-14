(define zero 
  (lambda (f) 
	(lambda (x) x)))

(define (add-1 n)
  (lambda (f) 
	(lambda (x) 
	  (f ((n f) x)))))

(define one 
  (lambda (f) 
	(lambda (x) 
	  (f x))))

(define two 
  (lambda (f) 
	(lambda (x)
	  (f (f x)))))

(define (add-church m n)
  (lambda (f)
	(lambda (x)
	  ((m f) ((n f) x)))))

;Direct definition of +: encapsulate x in m + n calls to f.


((zero 1+) 0)
((zero 1+) 1)
(((add-1 zero) 1+) 0)
((one 1+) 0)
((two 1+) 0)
(((add-church two two) 1+) 0)

