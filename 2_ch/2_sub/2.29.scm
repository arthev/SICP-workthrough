;;given
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))
(define (mobile? structure)
  (pair? structure))

(left-branch (make-mobile 2 3))
(right-branch (make-mobile 2 3))
(branch-length (make-branch 4 5))
(branch-structure (make-branch 4 5))


(define (total-weight mobile)
  (if (not (mobile? mobile)) 
	mobile
	(+ (total-weight (branch-structure (left-branch mobile)))
	   (total-weight (branch-structure (right-branch mobile))))))

(define (balanced? mobile)
  (define (branch-weight branch)
	(* (branch-length branch) (total-weight (branch-structure branch))))
  (if (not (mobile? mobile))
	true
	(let ((lb (left-branch mobile))
		  (rb (right-branch mobile)))
	  (and (= (branch-weight lb)
			  (branch-weight rb))
		   (balanced? (branch-structure lb))
		   (balanced? (branch-structure rb))))))




(define a (make-mobile (make-branch 4 12)
					   (make-branch 2 (make-mobile (make-branch 1 4)
												   (make-branch 6 2)))))
(total-weight a)
(balanced? a)

(define m1 (make-mobile (make-branch 1 2) (make-branch 2 1)))
(define m2 (make-mobile (make-branch 3 1) (make-branch 1 m1)))
(total-weight m2)
(balanced? m2)

