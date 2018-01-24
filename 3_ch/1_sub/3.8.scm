(define f
  (let ((lol 0))
	(lambda (x)
	  (cond ((= x 0) (set! lol -1) 0)
			((= x 1) (set! lol (1+ lol)) lol)))))

(+ (f 0) (f 1))
