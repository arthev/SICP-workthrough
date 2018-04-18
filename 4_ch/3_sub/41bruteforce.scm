(define (distinct? items)
  (cond ((null? items) true)
		((null? (cdr items)) true)
		((member (car items) (cdr items)) false)
		(else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (define pos (list 1 2 3 4 5))
  (define (baker floors)
	(if (null? floors)
	  #f
	  (let ((result (cooper pos (car floors))))
		(if result
		  result
		  (baker (cdr floors))))))
  (define (cooper floors baker)
	(if (null? floors)
	  #f
	  (let ((result (fletcher pos baker (car floors))))
		(if result
		  result
		  (cooper (cdr floors) baker)))))
  (define (fletcher floors baker cooper)
	(if (null? floors)
	  #f
	  (let ((result (miller pos baker cooper (car floors))))
		(if result
		  result
		  (fletcher (cdr floors) baker cooper)))))
  (define (miller floors baker cooper fletcher)
	(if (null? floors)
	  #f
	  (let ((result (smith pos baker cooper fletcher (car floors))))
		(if result
		  result
		  (miller (cdr floors) baker cooper fletcher)))))
  (define (smith floors baker cooper fletcher miller)
	(if (null? floors)
	  #f
	  (let ((result (solution baker cooper fletcher miller (car floors))))
		(if result
		  result
		  (smith (cdr floors) baker cooper fletcher miller)))))
  (define (solution baker cooper fletcher miller smith)
	(cond ((= baker 5)  #f)
		  ((= cooper 1) #f)
		  ((= fletcher 5) #f)
		  ((= fletcher 1) #f)
		  ((= (abs (- fletcher cooper)) 1) #f)
		  ((< miller cooper) #f)
		  ((= (abs (- smith fletcher)) 1) #f)
		  ((not (distinct? (list baker cooper fletcher miller smith))) #f)
		  (else (list (list 'baker baker)
					  (list 'cooper cooper)
					  (list 'fletcher fletcher)
					  (list 'miller miller)
					  (list 'smith smith)))))
  (baker pos))
(multiple-dwelling)
