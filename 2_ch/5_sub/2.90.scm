(define (negate x) (apply-generic 'negate x))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (the-empty-termlist) ((get 'the-empty-termlist 'sparse)))
(define (make-term order coeff) ((get 'make-term 'sparse) order coeff))
...
;Slight hack of just grabbing one when there's really no data to actually go by, because it's the initial step, so to say.
;The elegant solution would be to modify the polynomial package itself so it elects the type to lookup for those oddball cases
;depending on, say, the x of the (x . y) polynomials it's mathing away on.

(define (install-dense-package)
  ;;Internal procedures for representation of data
  (define (adjoin-term term term-list)
	(cond ((=zero? (coeff term)) term-list)
		  ((= (- (order term) (order (first-term term-list))) 1)
		   (cons (coeff order) term-list))
		  (else (adjoin-term term (cons 0 term-list)))))
  (define (the-empty-termlist) '())
  (define (first-term term-list) 
	(make-term (-1+ (length term-list)) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) 
	(null? (filter (lambda (x) (not (=zero? x)))) term-list))
  (define (make-term order coeff) (list order coeff)) 
  (define (order term) (car term)) 
  (define (coeff term) (cadr term)) 
  ;;Interface to the rest of the system
  (define (tag x) (attach-tag 'dense x))
  (put 'adjoin-term 'dense
	   (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'the-empty-termlist 'dense
	   (tag (the-empty-termlist)))
  (put 'first-term 'dense
	   (lambda (term-list) (tag (first-term term-list))))
  (put 'rest-terms 'dense
	   (lambda (term-list) (tag (rest-terms term-list))))
  (put 'empty-termlist? 'dense
	   empty-termlist?)
  (put 'make-term 'dense
	   make-term)
  (put 'order 'dense
	   order)
  (put 'coeff 'dense
	   coeff)
  'done)
(define adjoin-term-dense (get 'adjoin-term 'dense))
(define make-term-dense (get 'make-term 'dense))
;Because the dense and sparse package really share term-representation, the optimal solution might actually be a separate term package...


(define (install-sparse-package)
  (define (adjoin-term term term-list)
	(if (=zero? (coeff term))
	  term-list
	  (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list (car term-list)))
  (define (rest-terms term-list (cdr term-list)))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;;Interface
  (define (tag x) (attach-tag 'sparse x))
  (put 'adjoin-term 'sparse
	   (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'the-empty-termlist 'sparse
	   (tag (the-empty-termlist)))
  (put 'first-term 'sparse
	   first-term)
  (put 'rest-terms 'sparse
	   (lambda (term-list) (tag (rest-terms term-list))))
  (put 'empty-termlist? 'sparse
	   empty-termlist?)
  (put 'make-term 'sparse
	   make-term)
  (put 'order 'sparse
	   order)
  (put 'coeff 'sparse
	   coeff)
  'done)


(define (install-polynomial-package)
  ;;Internal procedures
  ;;Representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;;Representation of terms and term lists
    (define (add-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
	  (make-poly (variable p1)
				 (add-terms (term-list p1) (term-list p2)))
	  (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (add-terms L1 L2)
	(cond ((empty-termlist? L1) L2)
		  ((empty-termlist? L2) L1)
		  (else
			(let ((t1 (first-term L1))
				  (t2 (first-term L2)))
			  (cond ((> (order t1) (order t2))
					 (adjoin-term t1 (add-terms (rest-terms L1) L2)))
					((< (order t1) (order t2))
					 (adjoin-term t2 (add-terms L1 (rest-terms L2))))
					(else
					  (adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2)))
								   (add-terms (rest-terms L1) (rest-terms L2)))))))))
  (define (mul-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
	  (make-poly (variable p1)
				 (mul-terms (term-list p1) (term-list p2)))
	  (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (mul-terms L1 L2)
	(if (empty-termlist? L1)
	  (the-empty-termlist)
	  (add-terms (mul-terms-by-all-terms (first-term L1) L2)
				 (mul-terms (rest-terms L1) L2))))
  (define (mul-terms-by-all-terms t1 L)
	(if (empty-termlist? L)
	  (the-empty-termlist)
	  (let ((t2 (first-term L)))
		(adjoin-term (make-term (add (order t1) (order t2))
								(mul (coeff t1) (coeff t2)))
					 (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (negate-poly p)
	(make-poly (variable p)
			   (map (lambda (t) (make-term (order t) (negate (coeff t)))) 
					(term-list p))))
  ;;Interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
	   (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
	   (lambda (p1 p2) (tag (add-poly p1 (negate p2)))))
  (put 'mul '(polynomial polynomial)
	   (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
	   (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? 'polynomial
	   (lambda (p) (empty-termlist? (term-list p))))
  (put 'negate 'polynomial
	   negate-poly)
  'done)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (install-scheme-number-package)
  ...
  (put 'negate 'scheme-number
	   -)
  'done)

(define (install-rational-package)
  ...
  (put 'negate 'rational
	   (lambda (r) (make-rat (negate (numer x)) (denom x))))
  'done)

(define (install-complex-package)
  ...
  (put 'negate 'complex
	   (lambda (z) (make-from-real-imag (negate (real-part z1))
										(negate (imag-part z2)))))
  'done)
