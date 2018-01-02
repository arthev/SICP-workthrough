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

;2.81
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		(apply proc (map contents args))
		(if (= (length args) 2)
		  (let ((type1 (car type-tags))
				(type2 (cadr type-tags))
				(a1 (car args))
				(a2 (cadr args)))
			(let ((t1->t2 (get-coercion type1 type2))
				  (t2->t1 (get-coercion type2 type1)))
			  (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
					(t2->t1 (apply-generic op a1 (t2->t1 a2)))
					(else (error "No method for these types" (list op type-tags))))))
		  (error "No method for these types" (list op type-tags)))))))

;a)
;If apply-generic is called tith two arguments, for which the function is not defined in the table, 
;then it will fail to find the relevant function (since it doesn't exist). Having failed, 
;apply-generic will try to coerce the arguments, in this case, to their own current types, 
;and will then call itself anew with the 'new' types... Which will lead into an infinite recursion.

;b)
;Louis is not correct in that the coercion to same type is necessary:
;After all, it won't magically make a new function appear in the look-up table.
;After the coercion, apply-generic will just looks for the non-existant function again, and again and again and ...
;Apply-generic worked as it was already, though perhaps not in the most elegant way possible.

;c)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		(apply proc (map contents args))
		(if (= (length args) 2)
		  (let ((type1 (car type-tags))
				(type2 (cadr type-tags))
				(a1 (car args))
				(a2 (cadr args)))
			(if (not (equal? type1 type2))
			  (let ((t1->t2 (get-coercion type1 type2))
					(t2->t1 (get-coercion type2 type1)))
				(cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
					  (t2->t1 (apply-generic op a1 (t2->t1 a2)))
					  (else (error "No method for these types" (list op type-tags)))))
			  (error "No method for these types" (list op type-tags))))
		  (error "No method for these types" (list op type-tags)))))))

























