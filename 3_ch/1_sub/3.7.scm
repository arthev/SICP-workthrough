(define (make-account balance pwd)
  (define (withdraw amount)
	(if (>= balance amount)
	  (begin (set! balance (- balance amount))
			 balance)
	  "Insufficient funds"))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  (define (dispatch p m)
	(cond ((not (equal? p pwd)) (lambda (x) "Incorrect password"))
		  ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  ((eq? m 'correct-password?) #t)
		  (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)


(define (make-joint account org-pwd new-pwd)
  (if (equal? (account org-pwd 'correct-password?) "Incorrect password")
	"Incorrect password"
	(lambda (pwd msg)
	  (if (equal? new-pwd pwd)
		(account org-pwd msg)
		(account new-pwd msg)))))


(define peter-acc (make-account 1000 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((peter-acc 'open-sesame 'withdraw) 200)
((paul-acc 'rosebud 'withdraw) 400)





