;a
((lambda (n)
   ((lambda (fact) (fact fact n))
	(lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 10)

;There's no assignment being done, so we can walk through this using
;the substitution model.
;The expression creates a procedure where n = 10.
;Then the two body-lambdas get evaluated into procedures. So at that point we have something like...
;(procA<-(n=10)
;  (procB<-(fact=?)
;   procC<-(ft=? k=?)))
;
;Then, by the evaluation rules, we evaluate the operator - procB with procC as the operand, argument.
;(procA<-(n=10)
;  (procB<-(fact=procC<-(ft=? k=?))))
;
;ProcB then evaluates into (ProcC ProcC n), which means we get
;
;(ProcA<-(n=10)
;  (procC<-=(ft=procC k=10)))
;
;And from there on it's easy to see how the recursion works.

((lambda (n)
   ((lambda (fib) (fib fib n))
	(lambda (f k) (cond ((= k 0) 0)
						((= k 1) 1)
						(else 
						  (+ (f f (- k 1))
							 (f f (- k 2))))))))
 10)

;b)

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
	 (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
	 (if (= n 0) #f (ev? ev? od? (- n 1))))))


(f 4)

