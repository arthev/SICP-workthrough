(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

;(define (fast-mult a b)
;  (cond ((= b 1) a)
;		((even? b) (fast-mult (double a) (halve b)))
;		(else (+ a (fast-mult a (- b 1))))))

(define (fast-mult a b)
  (define (fast-mult-iter a b sub)
	(cond ((= b 1) (+ a sub))
		  ((even? b) (fast-mult-iter (double a) (halve b) sub))
		  (else (fast-mult-iter a (- b 1) (+ sub a)))))
  (fast-mult-iter a b 0))


(fast-mult 2 2)
(fast-mult 22 3)
(fast-mult 5 5)
(fast-mult 7 6)
(fast-mult 51 19)
(fast-mult 66 12)
(fast-mult 255 13)
(fast-mult 2 50)'
