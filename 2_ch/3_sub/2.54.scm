(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
		((and (pair? a) (pair? b)) (and (equal? (car a) (car b))
										(equal? (cdr a) (cdr b))))
		(else false)))



(equal? '(this list lol) '(this list lol))
(equal? 'lol 'lol)
(equal? 'lal 'lel)
(equal? '(this list) '(this list lol))
(equal? 'lel '(lality lolol))
