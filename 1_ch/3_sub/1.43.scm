

(define (compose f g)
  (lambda (x) (f (g x))))


;(define (repeated f n)
;(define (iter g n)
;(if (= n 1) g
;(iter (compose f g) (- n 1))))
;(iter f n))

(define (repeated f n)
  (if (= n 2)
	(compose f f)
	(compose f (repeated f (- n 1)))))

((repeated square 3) 5)
