;Library
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
		right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
	(list (symbol-leaf tree))
	(caddr tree)))
(define (weight tree)
  (if (leaf? tree)
	(weight-leaf tree)
	(cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
	(if (null? bits)
	  '()
	  (let ((next-branch (choose-branch (car bits) current-branch)))
		(if (leaf? next-branch)
		  (cons (symbol-leaf next-branch)
				(decode-1 (cdr bits) tree))
		  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
		((= bit 1) (right-branch branch))
		(else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else (cons (car set) (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
	'()
	(let ((pair (car pairs)))
	  (adjoin-set (make-leaf (car pair) ;symbol
							 (cadr pair)) ;frequency
				  (make-leaf-set (cdr pairs))))))

;2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
				  (make-code-tree
					(make-leaf 'B 2)
					(make-code-tree
					  (make-leaf 'D 1)
					  (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;(decode sample-message sample-tree)

;2.68
(define (encode message tree)
  (if (null? message)
	'()
	(append (encode-symbol (car message) tree)
			(encode (cdr message) tree))))
(define (encode-symbol x tree)
  (cond ((leaf? tree) '())
		((in? x (symbols (left-branch tree))) (cons 0 (encode-symbol x (left-branch tree))))
		((in? x (symbols (right-branch tree))) (cons 1 (encode-symbol x (right-branch tree))))
		(else (error "symbol not in tree: " symbol))))
(define (in? x set)
  (cond ((null? set) false)
		((equal? x (car set)) true)
		(else (in? x (cdr set)))))
;(encode '(a d a b b c a) sample-tree)

;2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge branches)
  (if (= (length branches) 1)
	(car branches)
	(successive-merge (adjoin-set (make-code-tree (car branches) (cadr branches))
								  (cddr branches)))))



(define sample-pairs (list (list 'A 8) (list 'B 3) (list 'C 1) (list 'D 1) (list 'E 1) (list 'F 1) (list 'G 1) (list 'H 1)))
(make-leaf-set sample-pairs)
(define sample-tree-2 (generate-huffman-tree sample-pairs))
(define sm2 (encode '(a b a d d c a h) sample-tree-2))
sm2
(encode '(a) sample-tree-2)
(decode sm2 sample-tree-2)
sample-tree-2

