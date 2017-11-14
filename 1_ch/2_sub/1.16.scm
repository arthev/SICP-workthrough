(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
		((even? n) (fast-expt-iter a 
								   (square b)
								   (/ n 2)))
		(else (fast-expt-iter (* a b)
							  b
							  (- n 1)))))



(fast-expt-iter 1 2 8)
(fast-expt-iter 1 3 3)
(fast-expt-iter 1 5 4)
(fast-expt-iter 1 1 44)
(fast-expt-iter 1 6 12)
(fast-expt-iter 1 9 15)
