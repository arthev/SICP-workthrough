(define (smallest-divisor n)
  (define (find-divisor test-divisor)
	(cond ((> (square test-divisor) n) n)
		  ((divides? test-divisor n) test-divisor)
		  (else (find-divisor (next test-divisor )))))
  (find-divisor 2))

(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
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


(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))



(define (fast-prime? n times)
  (cond ((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)))




(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
	(report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes start end)
  (cond ((< start end) 
		 (timed-prime-test start)
		 (search-for-primes (+ start 1) end))))


(search-for-primes 10000000000000000000000000000000000000000  10000000000000000000000000000000000100000)

