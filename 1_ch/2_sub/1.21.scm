(define (smallest-divisor n)
  (define (find-divisor test-divisor)
	(cond ((> (square test-divisor) n) n)
		  ((divides? test-divisor n) test-divisor)
		  (else (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))

(define (divides? a b) (= (remainder b a) 0))


(smallest-divisor 199)
;199 is a prime
(smallest-divisor 1999)
;1999 is a prime
(smallest-divisor 19999)
;19999 is not a prime, smallest divisor 7
(smallest-divisor 199999)
;199999 is a prime
