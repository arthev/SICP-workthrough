(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (square x) (* x x))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
(define (length-segment segment)
  (let ((x1 (x-point (start-segment segment)))
		(x2 (x-point (end-segment segment)))
		(y1 (y-point (start-segment segment)))
		(y2 (y-point (end-segment segment))))
	(sqrt (+ (square (abs (- x1 x2)))
			 (square (abs (- y1 y2)))))))

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

;(define (make-rectangle side1 side2) (cons side1 side2))
;(define (width-rectangle rect)
;  (let ((side1 (car rect))
;		(side2 (cdr rect)))
;	(if (< (length-segment side1) (length-segment side2))
;	  side1
;	  side2)))
;(define (length-rectangle rect)
;  (let ((side1 (car rect))
;		(side2 (cdr rect)))
;	(if (< (length-segment side1) (length-segment side2))
;	  side2
;	  side1)))

(define (make-rectangle side1 side2)
  (define (lowest-coordinate f)
	(min (f (start-segment side1))
		 (f (end-segment side1))
		 (f (start-segment side2))
		 (f (end-segment side2))))
  (let ((x0 (lowest-coordinate x-point))
		(y0 (lowest-coordinate y-point))
		(w (length-segment side1))
		(l (length-segment side2)))
	(cons (make-point x0 y0) (cons (make-segment (make-point 0 0)
												 (make-point 0 w))
								   (make-segment (make-point 0 0)
												 (make-point l 0))))))
(define (width-rectangle rect)
  (car (cdr rect)))
(define (length-rectangle rect)
  (cdr (cdr rect)))


(define (area-rectangle rect)
  (* (length-segment (width-rectangle rect))
	 (length-segment (length-rectangle rect))))

(define (perimeter-rectangle rect)
  (* 2 (+ (length-segment (width-rectangle rect))
		  (length-segment (length-rectangle rect)))))

(define r (make-rectangle (make-segment (make-point 1 1) (make-point 1 4))
						  (make-segment (make-point 1 1) (make-point 5 1))))

(area-rectangle r)
(perimeter-rectangle r)

















