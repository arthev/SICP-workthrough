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

(define (looperooper a b)
  (cond ((> a b) 0)
		(else (newline)
			  (display (cont-frac (lambda (i) 1.0)
								  (lambda (i) 1.0)
								  a))
			  (looperooper (1+ a) b))))

(looperooper 1 11)





