;;Downing is Lorna's father in the original variant.
;;For Mary Ann Whomever, there are two possible solutions.

(define (require p) (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
		((null? (cdr items)) true)
		((member (car items) (cdr items)) false)
		(else (distinct? (cdr items)))))

(define (list-amb li)
  (if (null? li)
	(amb)
	(amb (car li) (list-amb (cdr li)))))


(define filter
  (lambda (pred lst)
	(cond ((null? lst) '())
		  ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
		  (else (filter pred (cdr lst))))))

(define (map f lst)
  (if (null? lst)
	'()
	(cons (f (car lst)) (map f (cdr lst)))))


(define (yachtling)
  (define pos (list 'lorna 'melissa 'rosalind 'gabrielle 'maryann))
  (define (father r) (car r))
  (define (yacht r) (car (cdr r)))
  (define (daughter r) (car (cdr (cdr r))))
  (let ((hood (list 'hood 'gabrielle 'melissa 'maryann))
		(moore (list 'moore 'lorna (list-amb pos)))
		(hall (list 'hall 'rosalind (list-amb pos)))
		(downing (list 'downing 'melissa (list-amb pos)))
		(parker (list 'parker (list-amb pos) (list-amb pos))))
	(let ((rels (list hood moore hall downing parker)))
	  (require (distinct? (map yacht rels)))
	  (require (distinct? (map daughter rels)))
	  (require (null? (filter (lambda (r) (not (distinct? r))) 
							  rels)))
	  (require (equal? (daughter parker)
					   (yacht
						 (car
						   (filter (lambda (r) (equal? (daughter r) 'gabrielle)) 
								   rels)))))
	  (list hood moore hall downing parker))))

















