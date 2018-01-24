;The numbering-accounts-and-taking-the-lowest-first method avoids deadlock in the exchange
;situation because for any given exchange, to accounts need to be reserved. The deadlock
;scenario in the exchange situation arises when two or more exchanges are processed simultaneously,
;and need an already-acquired account to proceed. With the numbering-accounts-and-taking-the-lowest-first method,
;if an exchange requires access to an account which is already acquired... Then the exchange which has already
;acquired that account can only *additionally* require a higher-numbered account! Thus the deadlock is avoided.


(define (make-account-and-serializer balance)
  (let ((last-ID -1))
	(define (get-account-number)
	  (without-interrupts
		(lambda ()
		  (set! last-ID (1+ last-ID))
		  last-ID)))
	(define (withdraw amount)
	  (if (>= balance amount)
		(begin (set! balance (- balance amount))
			   balance)
		"Insufficient funds"))
	(define (deposit amount)
	  (set! balance (+ balance amount))
	  balance)
	(let ((balance-serializer (make-serializer))
		  (account-number (get-account-number)))
	  (define (dispatch m)
		(cond ((eq? m 'withdraw) withdraw)
			  ((eq? m 'deposit) deposit)
			  ((eq? m 'balance) balance)
			  ((eq? m 'serializer) balance-serializer)
			  ((eq? m 'account-number) account-number)
			  (else (error "Unknown request: MAKE-ACCOUNT" m))))
	  dispatch)))

(define (serialized-exchange account1 account2)
  (define (grab-in-order account1 account2)
	(let ((serializer1 (account1 'serializer))
		  (serializer2 (account2 'serializer)))
	  ((serializer1 (serializer2 exchange)) account1 account2)))
  (if (< (account1 'account-number) (account2 'account-number))
	(grab-in-order account1 account2)
	(grab-in-order account2 account1)))
