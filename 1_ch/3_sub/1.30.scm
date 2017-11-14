(define (cube x) (* x x x))
(define (square x) (* x x))

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

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpson-next k)
	(+ k 1))
  (define (simpson-term k)
	(define (yk)
	  (f (+ a (* k h))))
	(cond ((or (= k 0) (= k n)) (yk))
		  ((even? k) (* 2 (yk)))
		  (else (* 4 (yk)))))
  (* (sum simpson-term 0 simpson-next n)
	 (/ h 3)))


(simpson-integral cube 0 1 2)
(simpson-integral cube 0 1 4)
(simpson-integral square 0 1 2)
(simpson-integral square 0 1 4)
(simpson-integral square 0 1 6)
