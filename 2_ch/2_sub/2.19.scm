(define (cc amount coin-values)
  (define (first-denomination denominations)
	(car denominations))
  (define (except-first-denomination denominations)
	(cdr denominations))
  (define (no-more? denominations)
	(null? denominations))
  (cond ((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else (+ (cc amount
					 (except-first-denomination coin-values))
				 (cc (- amount
						(first-denomination coin-values))
					 coin-values)))))


(define us-coins (list 25 50 5 1 10))
(define uk-coins (list 0.5 1 2 100 50 20 10 5))

(cc 100 us-coins)
(cc 100 uk-coins)


;The order the coin values are given in does not affect the answer produced by cc.
;This makes sense on the heuristics level, because the amount of ways to make change
;for 51 cent is the same: 51 pennies, and 1 penny and a half-dollar. Whether you
;subtract the penny first or the half-dollar first doesn't affect the number of ways
;to make change. So that goes for correctness. We might well expect the calculation
;to take longer if we put small denominations first, however, since the expanded tree
;will be bigger. For each step down from the 51 pennies, the graph portion will go
;through all the other coins if we put the pennies first. With pennies last, there will
;be fewer generated nodes.
