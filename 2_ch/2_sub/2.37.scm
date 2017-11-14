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
;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
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
;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	nil
	(cons (accumulate op init (map car seqs))
		  (accumulate-n op init (map cdr seqs)))))
;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
(define (transpose mat)
  (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
	(map (lambda (row) (matrix-*-vector cols row)) m)))


(define A (list (list 0 4 -2) (list -4 -3 0)))
(define x (list 2 1 0))
(define B (list (list 0 1) (list 1 -1) (list 2 3)))

(matrix-*-matrix A B)
























