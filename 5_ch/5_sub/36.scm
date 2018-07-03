(load "ch5-compiler.scm")

(compile
  '(define (f x)
	 (+ x
		(g (+ 2 x))))
  'val
  'next)
