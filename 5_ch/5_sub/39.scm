;The environment is a list of frames, each of which is a list of variables.

(define (make-lexaddr frame-dis var-dis)
  (list frame-dis var-dis))

(define (lexaddr-framenum lexaddr)
  (car lexaddr))

(define (lexaddr-varnum lexaddr)
  (cadr lexaddr))


(define (lexical-address-lookup address env)
  (let* ((frame-num (lexaddr-framenum address))
		 (var-num (lexaddr-varnum address))
		 (the-frame (list-ref env frame-num))
		 (the-var (list-ref the-frame var-num))
		 (var-val (cadr the-var)))
	(if (eq? var-val '*unassigned*)
	  (error "LEX-ADDR-LOOKUP found *unassigned*" address env)
	  var-val)))

(define (lexical-address-set! value address env)
  (let* ((frame-num (lexaddr-framenum address))
		 (var-num (lexaddr-varnum address))
		 (the-frame (list-ref env frame-num))
		 (the-var (list-ref the-frame var-num)))
	(set-car! (cdr the-var) value)))

