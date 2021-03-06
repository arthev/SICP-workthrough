(define (square x) (* x x))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((proportion (monte-carlo 
					  trials 
					  (lambda ()
						(let ((test-x (random-in-range x1 x2))
							  (test-y (random-in-range y1 y2)))
						  (P test-x test-y)))))
		(width (- x2 x1))
		(height (- y2 y1)))
	(let ((area (* width height)))
	  (* proportion area))))

(define (random-in-range low high)
  (let ((range (- high low)))
	(+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
	(cond ((= trials-remaining 0)
		   (/ trials-passed trials))
		  ((experiment)
		   (iter (- trials-remaining 1)
				 (+ trials-passed 1)))
		  (else
			(iter (- trials-remaining 1)
				  trials-passed))))
  (iter trials 0))


(estimate-integral (lambda (x y) (<= (+ (square x) (square y)) 1))
				   -1.0 1.0
				   -1.0 1.0
				   533500)
