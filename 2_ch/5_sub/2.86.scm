;For our complex numbers to handle being made out of different
;numbers from our type system, we primarily need to change
;our complex numbers from assuming integers for their operations,
;and instead have operations carried out in terms of the generic
;operations. E.g. using the generic add instead of the presumptive
;+ function. Also, since cos, sin, sqrt and atan are presumably
;only implemented for normal scheme numbers, generic variants of
;these are also necessary.

(define (apply-generic op . args) (...))
(define (add x y) (drop (apply-generic 'add x y)))
(define (sub x y) (drop (apply-generic 'sub x y)))
(define (mul x y) (drop (apply-generic 'mul x y)))
(define (div x y) (drop (apply-generic 'div x y)))
...
(define (cosine x) (drop (apply-generic 'cosine x)))
(define (sine x) (drop (apply-generic 'sine x)))
(define (square-root x) (drop (apply-generic 'square-root x)))
(define (inverse-tangent x) (drop (apply-generic 'inverse-tangent x)))
(define (square x) (drop (apply-generic 'mul x x)))

(define (install-complex-package)
  ;;Import procedures from rectangular and polar packages
  ...
  ;;Internal procedures
  (define (add-complex z1 z2)
	(make-from-real-imag (add (real-part z1) (real-part z2))
						 (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
	(make-from-real-imag (sub (real-part z1) (real-part z2))
						 (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
	(make-from-mag-ang (mul (magnitude z1) (magnitude z2))
					   (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
	(make-from-mag-ang (div (magnitude z1) (magnitude z2))
					   (sub (angle z1) (angle z2))))
  ;;Interface to the rest of the system
  ...
  'done)

(define (install-rectangular-package)
  ;; Internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
	(square-root (add (square (real-part z))
					  (square (imag-part z)))))
  (define (angle z)
	(inverse-tangent (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
	(cons (mul r (cosine a))
		  (mul r (sine a))))
  ;;Interface to the rest of the system
  ...
  'done)
(define (install-polar-package)
  ;;Internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z) (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
	(cons (square-root (add (square x) (square y)))
		  (inverse-tangent y x)))
  ;;Interface to the rest of the system
  ...
  'done)

(define (install-rational-package)
  ;; Internal procedures
  ...
  ;; The new rational operations need implementations, heh. "TODO"
  (define (sin-rat x)
	(...)) ;Lazy way -> convert x to a real.
  (define (cos-rat x)
	(...)) ;See lazy way
  (define (sqrt-rat x)
	(make-rat (square-root (numer x))
			  (square-root (denom x))))
  (define (atan-rat y x)
	(...)) ;See lazy way
  ;;Interface to the rest of the system
  ...
  (put 'cosine 'rational
	   (lambda (x) (cos-rat x)))
  (put 'sine 'rational
	   (lambda (x) (sin-rat x)))
  (put 'square-root 'rational
	   (lambda (x) (sqrt-rat x)))
  (put 'inverse-tangent '(rational rational)
	   (lambda (y x) (atan-rat y x)))
  'done)

(define (install-scheme-number-package)
  ...
  (put 'cosine 'scheme-number cos)
  (put 'sine 'scheme-number sin)
  (put 'square-root 'scheme-number sqrt)
  (put 'inverse-tangent '(scheme-number scheme-number) atan)
  'done)

