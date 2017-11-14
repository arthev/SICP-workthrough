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

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
				(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
	(div-interval
	  one (add-interval (div-interval one r1)
						(div-interval one r2)))))





(define a (make-center-percent 12 0.2))
(define b (make-center-percent 6 0.3))
a
b
(newline)
(display "Now going with the a/b")
(newline)
(define c (par1 a b))
c
(center c)
(percent c)

(define d (par2 a b))
d
(center d)
(percent d)
(newline)
(display "Now going with the a/a and then b/b")
(newline)
(define e (par1 a a))
e
(center e)
(percent e)
(define f (par2 a a))
f
(center f)
(percent f)
(define g (par1 b b))
g
(center g)
(percent g)
(define h (par2 b b))
h
(center h)
(percent h)










































