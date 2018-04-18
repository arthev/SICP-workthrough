;;Simply replacing an-integer-between by an-integer-starting-from in the procedure from ex. 4.35
;;is not an adequate way to generate arbitrary pythagorean triplets because of the depth-first
;;search utilized in the amb evaluator. The evaluator would end up spending "all" its time searching
;;for triplets with numbers that don't make for any triplets, but simply keeping-on-keeping to try
;;higher and higher integers... 1^2 + 2^2 = 1000000000000^2????? Nope.


(define (a-pythagorean-triple)
  (let ((i (an-integer-starting-from 1)))
	(let ((j (an-integer-starting-from i)))
	  (require (>= (* i i) (- (* (1+ j) (1+ j)) 
							  (* j j))))
	  (let ((k (an-integer-starting-from j)))
		(require (> 10 (- k j)))
		(require (= (+ (* i i)
					   (* j j))
					(* k k)))
		(list i j k)))))


;Better solution, found online:
(define (a-pythagorean-triple-greater-than low)
  (let ((k (an-integer-starting-from low)))
	(let ((i (an-integer-between low k)))
	  (let ((j (an-integer-between i k)))
		(require (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))
