(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (square n) (* n n))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
		((even? exp)
		 (remainder
		   (square (expmod base (/ exp 2) m))
		   m))
		(else
		  (remainder
			(* base (expmod base (- exp 1) m))
			m))))

(define (search-for-primes start end)
  (cond ((< start end) 
		 (timed-prime-test start)
		 (search-for-primes (+ start 1) end))))

(define (carmichael? n a)
  (define (carmichael-test)
	(= (expmod a n n) a))
  (cond ((<= n a) true)
		((carmichael-test) (carmichael? n (next a)))
		(else false)))

(define (carmichael-outputter n)
  (newline)
  (display n)
  (if (carmichael? n 2) (display " *** passed the test")
	(display " *** failed the test"))
  0)


(carmichael-outputter 561)
(carmichael-outputter 1105)
(carmichael-outputter 1729)
(carmichael-outputter 2465)
(carmichael-outputter 2821)
(carmichael-outputter 6601)



