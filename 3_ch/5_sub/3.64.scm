;Library
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-ref s n)
  (if (= n 0)
	(stream-car s)
	(stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
	the-empty-stream
	(cons-stream (proc (stream-car s))
				 (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
	'done
	(begin (proc (stream-car s))
		   (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
	the-empty-stream
	(cons-stream low
				 (stream-enumerate-interval (1+ low) high))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
	the-empty-stream
	(cons-stream
	  (apply proc (map stream-car argstreams))
	  (apply stream-map
			 (cons proc (map stream-cdr argstreams))))))
;3.54
(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define (div-streams s1 s2) (stream-map / s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
			  stream))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))
;3.55
;(define (partial-sum S)
;  (cons-stream (stream-car S) (add-streams (stream-cdr S) (partial-sum S))))
;And from the web, more elegant:
(define (partial-sum S)
  (define ps (add-streams s (cons-stream 0 ps)))
  ps)
;3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
		  (let ((s1car (stream-car s1))
				(s2car (stream-car s2)))
			(cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
				  ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
				  (else
					(cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (scale-stream S 2) 
								(merge (scale-stream S 3) (scale-stream S 5)))))

;3.58
(define (expand num den radix)
  (cons-stream
	(quotient (* num radix) den)
	(expand (remainder (* num radix) den) den radix)))

;3.59
;a)
(define (integrate-series S)
  (mul-streams S
			   (div-streams ones integers)))
;and more elegantly still from the web...
(define (integrate-series S)
  (div-streams S integers))
;b)
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

;3.60
(define (mul-series s1 s2)
  (add-streams (scale-stream s1 (stream-car s2))
			   (cons-stream 0 (mul-series s1 (stream-cdr s2)))))
(define S (add-streams (mul-series sine-series sine-series)
					   (mul-series cosine-series cosine-series)))
;3.61
(define (invert-unit-series S)
  (define ps (cons-stream 1 (mul-series (scale-stream (stream-cdr S) -1)
										ps)))
  ps)
;3.62

(define (div-series dividend-series divisor-series)
  (let ((divisor-constant (stream-car divisor-series)))
	(if (= divisor-constant 0)
	  (error "div-series requires a divisor-series that is a power-series with constant term 1:" divisor-series)
	  (scale-stream (mul-series dividend-series 
								(invert-unit-series (scale-stream divisor-series (/ 1 divisor-constant))))
					divisor-constant))))
(define tangent-series (div-series sine-series cosine-series))

;3.64
(define (average x y) (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
	(cons-stream
	  1.0
	  (stream-map (lambda (guess) (sqrt-improve guess x))
				  guesses)))
  guesses)
(define (stream-limit s tol)
  (let ((a (stream-car s))
		(b (stream-car (stream-cdr s))))
	(if (< (abs (- a b)) tol)
	  b
	  (stream-limit (stream-cdr s) tol))))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
(sqrt 2 0.00001)












