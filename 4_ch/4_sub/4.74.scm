;a
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
  (stream-map stream-car
			  (stream-filter (lambda (s) (not (stream-null? s)))
							 stream)))
;b
;No. Given that Alyssa's observations are correct, then the
;only behaviour to the query system is that it might return valid
;results in a different order because of the removal of the interleaving.
