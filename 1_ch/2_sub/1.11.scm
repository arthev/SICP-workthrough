(define (fr n)
  (cond ((< n 3) n)
		(else (+ (fr (- n 1)) 
				 (* 2 (fr (- n 2)))
				 (* 3 (fr (- n 3)))))))

(define (fi n)
  (define (fi-iter current n1 n2 n3)
	(define new (+ n1 (* 2 n2) (* 3 n3)))
	(if (= current n) new
	  (fi-iter (+ current 1) new n1 n2)))
  (if (< n 3) n
	(fi-iter 3 2 1 0)))






(fr 0)
(fr 1)
(fr 2)
(fr 3)
(fr 4)
(fr 5)
(fr 6)
(fr 7)
(fr 8)
(fr 9)


(fi 0)
(fi 1)
(fi 2)
(fi 3)
(fi 4)
(fi 5)
(fi 6)
(fi 7)
(fi 8)
(fi 9)
