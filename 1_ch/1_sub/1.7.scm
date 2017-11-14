(define (sqrt-iter lastguess guess x)
  (if (good-enough? lastguess guess)
	guess
	(sqrt-iter guess (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? lastguess guess)
  (< (abs (- lastguess guess)) (/ lastguess 1000000)))

(define (sqrt x)
  (sqrt-iter x 1.0 x))

(sqrt 2)
(sqrt 0.005)
(sqrt 0.0000002)
(sqrt 5487193454215)

