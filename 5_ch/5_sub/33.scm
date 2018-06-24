(load "ch5-compiler.scm")

(compile
  '(define (factorial-alt n)
	 (if (= n 1)
	   1
	   (* n (factorial-alt (- n 1)))))
  'val
  'next)

