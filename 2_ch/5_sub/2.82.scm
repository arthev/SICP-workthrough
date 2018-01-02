;Library
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y)
  (if (not (eq? (type-tag x) (type-tag y)))
	#f
	(apply-generic 'equ? x y)))
(define (=zero? x) (apply-generic '=zero? x))


(define (install-rational-package)
  ;;internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
	(let ((g (gcd n d)))
	  (cons (/ n g) (/ d g))))
  (define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
				 (* (numer y) (denom x)))
			  (* (denom x) (denom y))))
  (define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
			  (* (denom x) (denom y))))
  (define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
				 (* (numer y) (denom x)))
			  (* (denom x) (denom y))))
  (define (div-rat x y)
	(make-rat (* (numer x) (denom y))
			  (* (denom x) (numer y))))
  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
	   (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
	   (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
	   (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
	   (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
	   (lambda (x y) (and (equ? (numer x) (numer y))
						  (equ? (denom x) (denom y)))))
  (put '=zero? 'rational
	   (lambda (x) (=zero? (numer x))))
  (put 'make 'rational
	   (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
	((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
	((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
	(make-from-real-imag (+ (real-part z1) (real-part z2))
						 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
	(make-from-real-imag (- (real-part z1) (real-part z2))
						 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
	(make-from-mag-ang (* (magnitude z1) (magnitude z2))
					   (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
	(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
					   (- (angle z1) (angle z2))))
  ;; interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
	   (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
	   (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
	   (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
	   (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
	   (lambda (z1 z2) (and (equ? (real-part z1) (real-part z2))
							(equ? (imag-part z1) (imag-part z2)))))
  (put '=zero? 'complex
	   (lambda (z) (and (=zero? (real-part z))
						(=zero? (imag-part z)))))
  (put 'make-from-real-imag 'complex
	   (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
	   (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
		((pair? datum) (car datum))
		(else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
		((pair? datum) (cdr datum))
		(else (error "Bad tagged datum: CONTENTS" datum))))

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number)
	   (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
	   (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
	   (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
	   (lambda (x y) (/ x y)))
  (put 'equ? '(scheme-number scheme-number)
	   =)
  (put '=zero? 'scheme-number
	   (lambda (x) (= x 0)))
  (put 'make 'scheme-number
	   (lambda (x) x))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;2.82

(define (apply-generic op . args)
  (define (find-unique-type-tags tags)
	(define (iter tags unique-tags)
	  (cond ((null? tags) unique-tags)
			((memq (car tags) unique-tags) iter (cdr tags) unique-tags)
			(else (iter (cdr tags) (cons (car tags) unique-tags))))) ;Assuming the order of the unique-tags won't be relevant.
	(iter tags '()))
  (define (find-coercion-target tags)
	(define (try current remaining n)
	  (cond ((null? remaining) current)
			((equal? current (car remaining)) (try current (cdr remaining) n))
			((get-coercion (car remaining) current) (try current (cdr remaining) n))
			((= (length tags) (1+ n)) #f)
			(else (try (list-ref tags (1+ n)) tags (1+ n)))))
	(try (list-ref tags 0) tags 0))
  (define (coerce-data data target)
	(if (null? data)
	  '()
	  (let ((current-type (type-tag data))
			(current (car data))
			(remaining-data (cdr data)))
		(if (equal? current-type target) 
		  (cons current (coerce-data remaining-data target))
		  (let ((current-type->target (get-coercion current-type target)))
			(if (current-type->target)
			  (cons (current-type->target current) (coerce-data remaining-data target))
			  (error "COERCE-DATA called with uncoercable types:" (list current-type target))))))))

  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		(apply proc (map contents args))
		(if (> (length args) 1)
		  (let ((coercion-target (find-coercion-target (find-unique-type-tags type-tags))))
			(if coercion-target
			  (apply-generic op (coerce-data args coercion-target))
			  (error "No way to coerce:" (list op type-tags target))))
		  (error "No method for these types:" (list op type-tags)))))))

;The solution is not general enough... Because if there is some suitable mixed-type operation, say, that X can be coerced to Y,
;while A can be coerced to B, and (op Y B) works, but X can't be coerced to B and A can't be coerced to Y, then it will fail.
;Aditionally, it will fail in the case where the arguments can all be coerced to the same type, but that type is not present in
;the type-tags of the current args.









