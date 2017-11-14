(define (make-interval a b) (cons a b))
(define (upper-bound x)
  (max (car x) (cdr x)))
(define (lower-bound x)
  (min (car x) (cdr x)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (define (p-percent-of-N p n)
	(* (/ n 100) p))
  (make-interval (- c (p-percent-of-N p c))
				 (+ c (p-percent-of-N p c))))
(define (percent i)
  (* 100 (+ -1 (/ (/ (upper-bound i) (center i)) 1.0 ))))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
				 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (define (spans-zero? i)
	(and (<= (lower-bound i) 0) (>= (upper-bound i) 0)))
  (if (spans-zero? y)
	(error "Division error (interval spans 0):" y)
	(mul-interval
	  x
	  (make-interval (/ 1.0 (upper-bound y))
					 (/ 1.0 (lower-bound y))))))


(percent (mul-interval (make-center-percent 5 2) (make-center-percent 3 2)))
(percent (mul-interval (make-center-percent 5 0.1) (make-center-percent 3 0.1)))
(percent (mul-interval (make-center-percent 5 0.5) (make-center-percent 3 0.4)))
(percent (mul-interval (make-center-percent 5 0.6) (make-center-percent 3 0.6)))
(percent (mul-interval (make-center-percent 5 0.3) (make-center-percent 3 0.5)))
(percent (mul-interval (make-center-percent 5 0.8) (make-center-percent 3 0.5)))
