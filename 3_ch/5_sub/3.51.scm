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


;3.51
(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
			  (stream-enumerate-interval 0 10)))
;x
(stream-ref x 5)
;0
;1
;2
;3
;4
;5
(stream-ref x 7)
;6
;7
