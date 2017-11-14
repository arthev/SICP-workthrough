;"Library" from subchapter
(define nil '())
(define square (lambda (x) (* x x)))
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
;2.38
(define (fold-right op initial sequence)
  (accumulate op initial sequence))
(define (fold-left op initial sequence)
  (define (iter result rest)
	(if (null? rest)
	  result
	  (iter (op result (car rest))
			(cdr rest))))
  (iter initial sequence))
;2.39
(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
;Nested mappings library
;Just bringing back a certain prime function from exercise 1.28, hehe.
(define (mrmod base exp m)
  (define (square-check n)
	(cond ((and (not (or (= n 1) (= n (- m 1))))
				(< (square n) m)
				(= (remainder (square n) m) 1))
		   0)
		  (else (square n))))
  (cond ((= exp 0) 1)
		((even? exp)
		 (remainder
		   (square-check (mrmod base (/ exp 2) m))
		   m))
		(else
		  (remainder
			(* base (mrmod base (- exp 1) m))
			m))))
(define (prime? n)
  (define (mr? times)
	(define (mr-test)
	  (define (try-it a)
		(= (mrmod a (- n 1) n) 1))
	  (try-it (+ 1 (random (- n 2)))))
	(cond ((= times 0) true)
		  ((mr-test) (mr? (- times 1)))
		  (else false)))
  (cond ((= n 0) false)
		((= n 1) false)
		((= n 2) true)
		(else (mr? 20))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
;(define (prime-sum-pairs n)
;  (map make-pair-sum
;	   (filter prime-sum?
;			   (flatmap (lambda (i) (map (lambda (j) (list i j))
;										 (enumerate-interval 1 (-1+ i))))
;						(enumerate-interval 1 n)))))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
		  sequence))
(define (permutations s)
  (if (null? s) ;empty set?
	(list nil)  ;sequence containing empty set
	(flatmap (lambda (x)
			   (map (lambda (p) (cons x p))
					(permutations (remove x s))))
			 s)))
;2.40
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
							(enumerate-interval 1 (-1+ i))))
		   (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (unique-pairs n))))
;2.41 -- original solution failed, looked online to see if others had same difficulty, found multiple other approaches instead.
(define (unique-triples n)
  (flatmap (lambda (k)
			 (map (lambda (pair)
					(cons k pair))
				  (unique-pairs (-1+ k))))
		   (enumerate-interval 1 n)))
(define (s-sum-triples n s)
  (filter (lambda (l) (= (accumulate + 0 l) s))
		  (unique-triples n)))
;2.42
















