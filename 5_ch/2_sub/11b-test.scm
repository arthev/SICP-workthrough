(load "11b.scm")

(define saver
  (make-machine
	'(b n)
	'()
	'(  (assign b (const 2))
	    (assign n (const 3))
		(save b)
		(restore n))))

(start saver)
(get-register-contents saver 'n)
