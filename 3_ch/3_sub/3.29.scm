;Library, heh
(define (inverter input output)
  (define (invert-input)
	(let ((new-value (logical-not (get-signal input))))
	  (after-delay
		inverter-delay
		(lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0 ) 1)
		((= s 1) 0)
		(else (error "Invalid signal" s))))
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
	(let ((new-value (logical-and (get-signal a1) (get-signal a2))))
	  (after-delay
		and-gate-delay
		(lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and s1 s2)
  (if (= (+ s1 s2) 2) 
	1
	0))
;3.29
;This or-gate's delay is INVERTER + AND + INVERTER 
;aka 2INVERTER + AND.
(define (or-gate a1 a2 output)
  (let ((i1 (make-wire))
		(i2 (make-wire))
		(anded (make-wire)))
	(inverter a1 i1)
	(inverter a2 i2)
	(and-gate i1 i2 anded)
	(inverter anded output)
	'ok))











