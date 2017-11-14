
(define (make-interval a b) (cons a b))
(define (upper-bound x)
  (max (car x) (cdr x)))
(define (lower-bound x)
  (min (car x) (cdr x)))




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


(define a (make-interval -2 4))
(define b (make-interval 2 5))
(define c (div-interval b a))
