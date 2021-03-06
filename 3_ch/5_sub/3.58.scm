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

(define a (expand 11 8 10))
(stream-ref a 0)
(stream-ref a 1)
(stream-ref a 2)
(stream-ref a 3)
(stream-ref a 4)

;The expand stream is the numbers behind the radix pointt for a given base number (radix). (And if you input num > den, the first
;number in the stream will be on the format xxxxa where a is the first number behind the radix point, heh.
;AKA the rational value of dividing num by den in base radix.



























