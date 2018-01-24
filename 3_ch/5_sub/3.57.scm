(define fibs
  (cons-stream
	0
	(cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
;To compute the nth fib number, given the previous computation of n-1th in the stream...
;There's a total of one addition carried out.

;If we hadn't used memo-proc then we would not be able to lookup the previous numbers computed,
;and would have to recompute those for the generation of each nth fib number. And as we recall
;from earlier chapters of the book, tree-generation of fib numbers with no memoization occurs
;in exponential time with base phi.

;Number of additions for computing the nth fib number is fib(n) - 1.
