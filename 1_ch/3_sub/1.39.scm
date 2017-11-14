(define (cube x) (* x x x))
(define (square x) (* x x))
(define (inc n) (1+ n))
(define (identity n) n)

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (combiner result (term a)))))
  (iter a null-value))


;(define (cont-frac n d k)
;  (define (recurser a)
;	(if (> a k)
;	  0
;	  (/ (n a) (+ (d a) (recurser (1+ a))))))
;  (recurser 1))

(define (cont-frac n d k)
  (define (iter k result)
	(if (= k 0)
	  result
	  (iter (- k 1) (/ (n k)
					   (+ (d k) result)))))
  (iter k 0))


(define (tan-cf x k)
  (cont-frac (lambda (i) (if (> i 1) (- (square x))
						   x))
			 (lambda (i) (+ i (- i 1)))
			 k))



(tan-cf 0.0 20)
(tan-cf 0.3 20)
(tan-cf 1.5 20)
(tan-cf -0.6 20)


