;;The two functions in question are:
(define (append x y)
  (if (null? x)
	y
	(cons (car x) (append (cdr x) y))))
(define (append! x y)
  (define (last-pair x)
	(if (null? (cdr x)) x (last-pair (cdr x))))
  (set-cdr! (last-pair x) y)
  x)
;;However, the append function is a bit odd. Since we aren't doing 
;;a mutable append, presumably we want a new list as a result. But
;;the append uses an old list as part of the new list.
;;E.g. consider the following:
;;(define a (list 1 2 3))
;;(define b (list 4 5 6))
;;(define c (append a b))
;;(set-car! b 7)
;;c
;;Value nn: (1 2 3 7 5 6)
;;Therefore, I will instead make a machine for the following function:
(define (append x y)
  (cond ((and (null? x) (null? y)) '())
		((null? x) (cons (car y) (append x (cdr y))))
		(else (cons (car x) (append (cdr x) y)))))
(load "regsim.scm")

(define APP
  (make-machine
	'(result x y v continue)
	(list (list 'cons cons) (list 'car car) (list 'cdr cdr) (list 'null? null?))
	'(  (assign continue (label APP-done))
	    (assign v (const 0))
		(assign result (const ()))
		(save continue)
		(save v)
	  x-loop
	    (test (op null?) (reg x))
		(branch (label y-loop))
		(assign continue (label conser))
		(assign v (op car) (reg x))
		(save continue)
		(save v)
		(assign x (op cdr) (reg x))
		(goto (label x-loop))
	  y-loop
	    (test (op null?) (reg y))
		(branch (label collapser))
		(assign continue (label conser))
		(assign v (op car) (reg y))
		(save continue)
		(save v)
		(assign y (op cdr) (reg y))
		(goto (label y-loop))
	  conser
	  	(assign result (op cons) (reg v) (reg result))
	  collapser
	    (restore v)
		(restore continue)
		(goto (reg continue))
	  APP-done)))

(define APP!
  (make-machine
	'(retval x y temp)
	(list (list 'car car) (list 'cdr cdr) (list 'set-cdr! set-cdr!) (list 'null? null?))
	'(  (assign retval (reg x))
	  APP!-loop
	    (assign temp (op cdr) (reg x))
		(test (op null?) (reg temp))
		(branch (label APP!-mutate))
		(assign x (reg temp))
		(goto (label APP!-loop))
	  APP!-mutate
	    (perform (op set-cdr!) (reg x) (reg y))
	  APP!-done)))


(define a (list 1 2 3))
(define b (list 4 5 6))
(set-register-contents! APP 'x a)
(set-register-contents! APP 'y b)
(set-register-contents! APP! 'x a)
(set-register-contents! APP! 'y b)
(start APP)
(get-register-contents APP 'result)
(start APP!)
(get-register-contents APP! 'retval)
(set-car! b 7)
(get-register-contents APP! 'retval)


