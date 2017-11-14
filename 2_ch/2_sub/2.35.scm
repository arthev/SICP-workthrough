;"Library" from subchapter
(define nil '())
(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if  (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
	'()
	(cons low (enumerate-interval (1+ low) high))))
(define (enumerate-tree tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
					  (enumerate-tree (cdr tree))))))
;2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
				(+ this-coeff
				   (* x higher-terms)))
			  0
			  coefficient-sequence))
;2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves (list))
(count-leaves (list 1 2 3))
(count-leaves (list 1 (list 1 2 3)))
(count-leaves (list (list 1 2) (list 1 2 3) 1))























