(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (let ((x1 (x-point (start-segment segment)))
		(x2 (x-point (end-segment segment)))
		(y1 (y-point (start-segment segment)))
		(y2 (y-point (end-segment segment))))
	(make-point (/ (+ x1 x2) 2) 
				(/ (+ y1 y2) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))


(define a (make-point 2 -4))
(define b (make-point 8 -10))
(define s (make-segment a b))
(print-point (midpoint-segment s))
