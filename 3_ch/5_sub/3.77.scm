;Library
(define square (lambda (x) (* x x)))
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
(define (display-n-lines-stream s n)
  (if (= n 0)
	"done"
	(begin
	  (display-line (stream-car s))
	  (display-n-lines-stream (stream-cdr s) (- n 1)))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
		((pred (stream-car stream))
		 (cons-stream (stream-car stream)
					  (stream-filter pred (stream-cdr stream))))
		(else (stream-filter pred (stream-cdr stream)))))


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

;3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
			   (stream-map - (ln2-summands (1+ n)))))
(define ln2-stream
  (partial-sum (ln2-summands 1)))
;(display-n-lines-stream ln2-stream 10)
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
		(s1 (stream-ref s 1))
		(s2 (stream-ref s 2)))
	(cons-stream (- s2 (/ ((lambda (x) (* x x)) (- s2 s1))
						  (+ s0 (* -2 s1) s2)))
				 (euler-transform (stream-cdr s)))))
;(display-n-lines-stream (euler-transform ln2-stream) 10)
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
;(display-n-lines-stream (accelerated-sequence euler-transform ln2-stream) 10)

;3.66
(define (interleave s1 s2)
  (if (stream-null? s1)
	s2
	(cons-stream (stream-car s1)
				 (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
	(list (stream-car s) (stream-car t))
	(interleave
	  (stream-map (lambda (x) (list (stream-car s) x))
				  (stream-cdr t))
	  (pairs (stream-cdr s) (stream-cdr t)))))

;3.69
(define (triples s t u)
  (newline)
  (display "Now we're inside the triples call again.")
  (cons-stream
	(list (stream-car s) (stream-car t) (stream-car u))
	(interleave
	  (interleave
		(stream-map (lambda (x) (merge (stream-car s) x))
					(stream-map (lambda (x) (list (stream-car t) x))
								(stream-cdr u)))
		(triples s t (stream-cdr u)))
	  (interleave
		(triples s (stream-cdr t) (stream-cdr u))
		(triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define (triples s t u)
  (cons-stream
	(list (stream-car s) (stream-car t) (stream-car u))
	(interleave
	  (stream-map (lambda (x) (cons (stream-car s) x))
				  (stream-cdr (pairs t u)))
	  (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

;3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
		  (let ((s1car (stream-car s1))
				(s2car (stream-car s2)))
			(if (<= (weight s1car) (weight s2car))
			  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
			  (merge-weighted s2 s1 weight))))))

(define (weighted-pairs s t weight)
  (cons-stream
	(list (stream-car s) (stream-car t))
	(merge-weighted
	  (stream-map (lambda (x) (list (stream-car s) x))
				  (stream-cdr t))
	  (pairs (stream-cdr s) (stream-cdr t))
	  weight)))


;(define a (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))
;(display-n-lines-stream a 10)
;(define b (stream-filter (lambda (pair) (let ((i (car pair))
;											  (j (cadr pair)))
;										  (not (or
;												 (= (remainder i 2) 0)
;												 (= (remainder j 2) 0)
;												 (= (remainder i 3) 0)
;												 (= (remainder j 3) 0)
;												 (= (remainder i 5) 0)
;												 (= (remainder j 5) 0)))))
;						 (weighted-pairs 
;						   integers 
;						   integers
;						   (lambda (x) (let ((i (car x))
;											 (j (cadr x)))
;										 (+ (* 2 i) (* 3 j) (* 5 i j)))))))
;(display-n-lines-stream b 10)

;3.71
(define (cube x) (* x x x))
(define (ram-weight x)
  (+ (cube (car x)) (cube (cadr x))))
(define potential-rams 
  (weighted-pairs integers integers ram-weight))
(define (ram-filter stream)
  (let ((a (stream-car stream))
		(b (stream-car (stream-cdr stream))))
	(if (= (ram-weight a) (ram-weight b))
	  (cons-stream (list (ram-weight a) a b)
				   (ram-filter (stream-cdr (stream-cdr stream))))
	  (ram-filter (stream-cdr stream)))))

;(display-stream (ram-filter potential-rams))

;3.72
(define (ram-triples-filter stream)
  (let ((a (stream-car stream))
		(b (stream-car (stream-cdr stream)))
		(c (stream-car (stream-cdr (stream-cdr stream)))))
	(if (= (ram-weight a)
		   (ram-weight b)
		   (ram-weight c))
	  (cons-stream (list (ram-weight a) a b c)
				   (ram-triples-filter (stream-cdr (stream-cdr (stream-cdr stream)))))
	  (ram-triples-filter (stream-cdr stream)))))

;3.73
(define (integral integrand initial-value dt)
  (define int
	(cons-stream initial-value
				 (add-streams (scale-stream integrand dt)
							  int)))
  int)

(define (RC R C dT)
  (lambda (iStream v0)
	(add-streams
	  (scale-stream iStream R)
	  (integral (scale-stream iStream (/ 1.0 C))
				v0
				dT))))


(define RC1 (RC 5 1 0.5))
;(display-n-lines-stream (RC1 integers 0.2) 5)

;3.74
(define zero-crossings
  (stream-map sign-change-detector
			  sense-data
			  (stream-cdr sense-data)))

;3.75
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
					last-value)
				 2)))
	(cons-stream
	  (sign-change-detector avpt last-avpt)
	  (make-zero-crossings
		(stream-cdr input-stream) (stream-car input-stream) avpt))))

;3.76
(define (smooth s)
  (stream-map average
			  s
			  (stream-cdr s)))
(define (make-zero-crossings sense-data
							 (stream-map sign-change-detector
										 sense-data
										 (stream-cdr sense-data))))
(define szc (make-zero-crossings (smooth sense-data)))

;3.77
(define (integral delayed-integrand initial-value dt)
  (define int
	(cons-stream
	  initial-value
	  (let ((integrand (force delayed-integrand)))
		(add-streams (scale-stream integrand dt)
					 int))))
  int)

(define (integral delayed-integrand initial-value dt)
  (cons-stream
	initial-value
	(let ((integrand (force delayed-integrand)))
	  (if (stream-null? integrand)
		the-empty-stream
		(integral (delay (stream-cdr integrand))
				  (+ (* dt (stream-car integrand))
					 initial-value)
				  dt)))))


(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)





















