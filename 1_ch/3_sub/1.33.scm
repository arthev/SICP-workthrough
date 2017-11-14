(define (cube x) (* x x x))
(define (square x) (* x x))
(define (inc n) (1+ n))
(define (identity n) n)
(define (true!) true)
(define (next n)
  (if (= n 2) 3 (+ n 2)))


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

(define (prime? n)
  (define (mr? times)
	(define (mr-test)
	  (define (try-it a)
		(= (mrmod a (- n 1) n) 1))
	  (try-it (+ 1 (random (- n 2)))))
	(cond ((= times 0) true)
		  ((mr-test) (mr? (- times 1)))
		  (else false)))
  (if (= n 2) true
	(mr? 100)))

(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter a result)
	(cond ((> a b) result)
		  ((predicate a) (iter (next a) (combiner result (term a))))
		  (else (iter (next a) result))))
  (iter a null-value))

;(define (accumulate combiner null-value term a next b)
;  (define (iter a result)
;	(if (> a b)
;	  result
;	  (iter (next a) (combiner result (term a)))))
;  (iter a null-value))

;(define (accumulate combiner null-value term a next b)
;  (filtered-accumulate true! combiner null-value term a next b))

;(define (sum term a next b)
;  (accumulate + 0 term a next b))

;(define (product term a next b)
;  (accumulate * 1 term a next b))

;(define (simple-sum a b)
;  (sum identity a 1+ b))

;(define (factorial n)
;  (product identity 1 1+ n))

;(simple-sum 1 100)
;(factorial 5)
;(factorial 6)

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate prime? + 0 square a 1+ b))


(sum-of-squares-of-primes 2 100)

(define (product-of-relative-primes n)
  (define (relative-prime? i)
	(= (gcd i n) 1))
  (filtered-accumulate relative-prime? * 1 identity 1 1+ n))


(product-of-relative-primes 10)
