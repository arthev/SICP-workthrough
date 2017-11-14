(define (last-pair items)
  (if (null? (cdr items))
	items
	(last-pair (cdr items))))

;(define (last-pair items)
;  (list (list-ref items (-1+ (length items))))) 

(last-pair (list 23 72 179 34))
