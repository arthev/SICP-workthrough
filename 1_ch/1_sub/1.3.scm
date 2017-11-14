(define (square x) (* x x))

(define (square-biggest-two x y z)
  		(cond ((<= x y z) (+ (square y) (square z)))
			  ((<= y x z) (+ (square x) (square z)))
			  (else (+ (square x) (square z)))))

(square-biggest-two -4 5 6)
(square-biggest-two -4 -5 6)
(square-biggest-two 22 5 13)
(square-biggest-two 50 999 100)
