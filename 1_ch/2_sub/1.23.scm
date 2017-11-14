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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
	(report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes start end)
  (cond ((< start end) 
		 (timed-prime-test start)
		 (search-for-primes (+ start 1) end))))


(search-for-primes 1000000000000 1000000000065)

