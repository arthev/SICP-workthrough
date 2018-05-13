(load "9.scm")

(define labeler
  (make-machine
	'(a)
	(list (list 'p (lambda l
					(map
					  (lambda (i)
						(newline)
						(display i)
						5)
					  l))))
	'(start
	   (assign a (const 2))
	  test-label
	   (assign a (op p) (const 3) (reg a) (label test-label))
	  done)))

(start labeler)
(get-register-contents labeler 'a)
