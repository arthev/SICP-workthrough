(load "load-eceval-compiler.scm")
(load "mceval-text.scm")

(newline)(display "Now messing with primitives...")(newline)

(append! 
  primitive-procedures 
  (list
	(list 'apply (lambda (proc args) (apply (primitive-implementation proc) args)))
	
	;Listiness
	(list 'list list)
	(list 'car car)
	(list 'cdr cdr)
	(list 'cadr cadr)
	(list 'cddr cddr)
	(list 'caddr caddr)
	(list 'cdddr cdddr)
	(list 'cadddr cadddr)
	(list 'cddddr cddddr)
	(list 'cdadr cdadr)
	(list 'caadr caadr)
	(list 'not not)
	(list 'null? null?)
	(list 'eq? eq?)
	(list 'length length)
	(list 'set-car! set-car!)
	(list 'set-cdr! set-cdr!)

	;Misc
	(list 'number? number?)
	(list 'pair? pair?)
	(list 'symbol? symbol?)
	(list 'string? string?)
	(list 'error error)


	;Required by driver-loop
	(list 'newline newline)
	(list 'display display)
	(list 'read read)
	(list 'setup-environment setup-environment)
	
	
	
	
	))

(define the-global-environment (setup-environment))

(newline)(display "now doing the compile-and-go...")(newline)
(compile-and-go 
  `(begin
	 (define apply-in-underlying-scheme apply)
	 ,mceval-text
	 (define (map proc items)
	   (if (null? items)
		 '()
		 (cons (proc (car items))
			   (map proc (cdr items)))))
	 (define the-global-environment (setup-environment))
	 (driver-loop)
	 ))

;Works for (define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n)))).
