(define (make-table same-key?)
  (let ((local-table (list '*table*)))
	(define (assoc key records)
	  (cond ((null? records) false)
			((same-key? key (caar records)) (car records))
			(else (assoc key (cdr records)))))
	(define (lookup keys)
	  (define (looper keys table)
		(let ((subtable (assoc (car keys) (cdr table))))
		  (if subtable
			(if (= (length keys) 1)
			  (cdr subtable)
			  (looper (cdr keys) subtable))
			false)))
	  (looper keys local-table))
	(define (insert! keys value)
	  (define (set-subtable-structures! keys table)
		(define (keyhandler keys)
		  (if (= (length keys) 1)
			(cons (car keys) value)
			(list (car keys) (keyhandler (cdr keys)))))
		(set-cdr! table (cons (keyhandler keys)
							  (cdr table))))
	  (define (looper keys table)
		(let ((subtable (assoc (car keys) (cdr table))))
		  (if subtable
			(if (= (length keys) 1)
			  (set-cdr! subtable value)
			  (looper (cdr keys) subtable))
			(set-subtable-structures! keys table))))
	  (looper keys local-table)
	  'ok)
	(define (dispatch m)
	  (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			((eq? m 'view) local-table)
			(else (error "Unknown operation: TABLE" m))))
	dispatch))
(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;If the ability to both use a key to store a data in N dimensions AND to use the key as part of an M-dimensional keychain, where M > N,
;then this needs some special storage method for the N-dimensional key. Probably storing it in the table header somehow, by making
;a different data structure there than a simple pair consisting of the keyname and pointer to the record list. Eg. a pair whose car
;is a pair whose car is the keyname and whose cdr is the potential value for the N-dimensional key, and whose cdr is the pointer
;to the record list.

(put (list 'a) 1)
(put (list 'b 'c) 2)
(put (list 'c 'd 'e) 3)
(put (list 'd 'e 'f 'g) 4)
(operation-table  'view)
(get (list 'd 'e 'f 'g))
(get (list 'c 'd 'e))
(get (list 'b 'c))
(get (list 'a))

