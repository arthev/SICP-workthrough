(define (cube x) (* x x x))
(define (square x) (* x x))
(define (inc n) (1+ n))
(define (identity n) n)
;(define (sum term a next b)
;  (if (> a b)
;	0
;	(+ (term a)
;	   (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (+ result
						(term a)))))
  (iter a 0))

;(define (product term a next b)
;  (if (> a b)
;	1
;	(* (term a)
;	   (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (* result
						(term a)))))
  (iter a 1))

;(define (simpson-integral f a b n)
;  (define h (/ (- b a) n))
;  (define (simpson-next k)
;	(+ k 1))
;  (define (simpson-term k)
;	(define (yk)
;	  (f (+ a (* k h))))
;	(cond ((or (= k 0) (= k n)) (yk))
;		  ((even? k) (* 2 (yk)))
;		  (else (* 4 (yk)))))
;  (* (sum simpson-term 0 simpson-next n)
;	 (/ h 3)))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-product n)
  (define (numerator-term a)
	(if (even? a) (+ a 2)
	  (1+ a)))
  (define (denominator-term a)
	(if (even? a) (1+ a)
	  (+ a 2)))
  (* 4.0 (/ (product numerator-term 1 inc n)
			(product denominator-term 1 inc n))))


(factorial 6)



(pi-product 1)
(pi-product 2)
(pi-product 3)
(pi-product 4)
(pi-product 5)
(pi-product 6)
(pi-product 7)
(pi-product 8)
(pi-product 9)
(pi-product 10)
(pi-product 1000)
(pi-product 10000)
(pi-product 100000)
