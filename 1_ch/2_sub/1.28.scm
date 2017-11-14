(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (square n) (* n n))

(define (mrmod base exp m)
  (define (square-check n)
	(cond ((and (not (or (= n 1) (= n (- m 1))))
				(< (square n) m)
				(= (remainder (square n) m) 1))
		   0)
		  (else (square n))))
  (cond ((= exp 0) 1)
		((even? exp)
		 (remainder
		   (square-check (mrmod base (/ exp 2) m))
		   m))
		(else
		  (remainder
			(* base (mrmod base (- exp 1) m))
			m))))

(define (mr? n times)
  (define (mr-test)
	(define (try-it a)
	  (= (mrmod a (- n 1) n) 1))
	(try-it (+ 1 (random (- n 2)))))
  (cond ((= times 0) true)
		((mr-test) (mr? n (- times 1)))
		(else false)))


(define (prime? n)
  (newline)
  (display n)
  (if (mr? n 100) (display " *** is prime")
	(display " *** is NOT prime"))
  0)


(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)

(prime? 3)
(prime? 5)
(prime? 7)
(prime? 11)
(prime? 13)
(prime? 17)
(prime? 19)
(prime? 23)



(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (mr? n 100)
	(report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond ((< start end)
		 (timed-prime-test start)
		 (search-for-primes (+ start 1) end))))



(prime? 0)
(prime? 1)
(prime? 2)
(prime? 3)














