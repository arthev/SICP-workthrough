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
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
	(let ((new-value (logical-or (get-signal a1) (get-signal a2))))
	  (after-delay 
		or-gate-delay
		(lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
(define (logical-or s1 s2)
  (if (> (+ s1 s2) 0)
	1
	0))
;3.30
(define (half-adder a b s c)
  (let ((d (make-wire))
		(e (make-wire)))
	(or-gate a b d)
	(and-gate a b c)
	(inverter c e)
	(and-gate d e s)
	'ok))
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
		(c1 (make-wire))
		(c2 (make-wire)))
	(half-adder b c-in s c1)
	(half-adder a s sum c2)
	(or-gate c1 c2 c-out)
	'ok))
(define (ripple-carry-adder Alist Blist Slist Cout)
  (define (looper Alist Blist Slist Cin)
	(if (= (length Slist) 1)
	  (begin
		(full-adder (car Alist) (car Blist) Cin (car Slist) Cout)
		'ok)
	  (begin
		(let ((Cnext (make-wire)))
		  (full-adder (car Alist) (car Blist) Cin (car Slist) Cnext)
		  (looper (cdr Alist) (cdr Blist) (cdr Slist) Cnext)))))
  (if (= (length Alist) (length Blist) (length Slist))
	(let ((Cn (make-wire)))
	  (set-signal! Cn 0)
	  (looper (reverse Alist) (reverse Blist) (reverse Slist) Cn))
	(error "ripple-carry-adder with unequal Alist Blist Slist attempted:" (list Alist Blist Slist))))
;An N-bit ripple-carry adder consists of N full-adders.
;A full-adder's longest path is through its two half-adders.
;A half-adder's longest path is most likely AND-INVERT-AND.
;Thus a half-adder is (2 * AND + INVERT),
;and a full-adder is 2 * (2 * AND + INVERT) aka
;(4*AND + 2*INVERT), so the N-bit ripple-carry adder's
;delay becomes N*(4*AND + 2*INVERT).






















































