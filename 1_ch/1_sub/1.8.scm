(define (cbrt-iter lastguess guess x)
  (if (good-enough? lastguess guess)
	guess
	(cbrt-iter guess (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x 
		   (* guess guess)) 
		(* 2 guess)) 
	 3))

(define (good-enough? lastguess guess)
  (< (abs (- lastguess guess)) (/ lastguess 1000000)))

(define (cbrt x)
  (cbrt-iter x 1.0 x))

(cbrt 9)
(cbrt 81)
(cbrt 2)
(cbrt 0.005)
(cbrt 0.0000002)
(cbrt 5487193454215)

