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
(define a (partial-sum (stream-enumerate-interval 1 10)))
(display-stream a)































