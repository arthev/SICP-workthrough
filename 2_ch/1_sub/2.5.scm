;(define (log3 n)
;  (/ (log n) (log 3)))
;(define (div2num n)
;  (define (iter n result)
;	(if (not (= (remainder n 2) 0))
;	  result
;	  (iter (/ n 2) (1+ result))))
;  (iter n 0))
;(define (cons a b)
;  (* (expt 2 a)
;	 (expt 3 b)))
;(define (car p)
;  (div2num p))
;(define (cdr p)
;  (log3 (/ p (expt 2 (div2num p)))))


(define (divmnum n m)
  (define (iter n result)
	(if (not (= (remainder n m) 0))
	  result
	  (iter (/ n m) (1+ result))))
  (iter n 0))
(define (cons a b)
  (* (expt 2 a)
	 (expt 3 b)))
(define (car p)
  (divmnum p 2))
(define (cdr p)
  (divmnum p 3))




(define z (cons 4 3))
z
(car z)
(cdr z)


