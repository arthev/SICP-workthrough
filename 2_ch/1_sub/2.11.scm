
(define (make-interval a b) (cons a b))
(define (upper-bound x)
  (max (car x) (cdr x)))
(define (lower-bound x)
  (min (car x) (cdr x)))




(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
				 (- (upper-bound x) (lower-bound y))))

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (define (pos? n)
	(>= n 0))
  (define (neg? n)
	(< n 0))
  (define (spans-0? n m)
	(and (>= (max n m) 0) (< (min n m) 0)))
  (let ((xl (lower-bound x))
		(yl (lower-bound y))
		(xu (upper-bound x))
		(yu (upper-bound y)))
	(cond ((and (pos? xl) (pos? yl)) 				(make-interval (* xl yl) (* xu yu)))
		  ((and (pos? xl) (spans-0? yl yu)) 		(make-interval (* xu yl) (* xu yu)))
		  ((and (pos? xl) (neg? yu)) 				(make-interval (* xu yl) (* xl yu)))
		  ((and (neg? xu) (pos? yl)) 				(make-interval (* xl yu) (* xu yl)))
		  ((and (neg? xu) (spans-0? yl yu)) 		(make-interval (* xl yu) (* xl yl)))
		  ((and (neg? xu) (neg? yu)) 				(make-interval (* xu yu) (* xl yl)))
		  ((and (spans-0? xl xu) (pos? yl)) 		(make-interval (* xl yu) (* xu yu)))
		  ((and (spans-0? xl xu) (spans-0? yl yu)) 	(make-interval (min (* xl yu) (* xu yl)) (max (* xl yl) (* xu yu))))
		  ((and (spans-0? xl xu) (neg? yu)) 		(make-interval (* xu yl) (* xl yl))))))
		  



(define (div-interval x y)
  (define (spans-zero? i)
	(and (<= (lower-bound i) 0) (>= (upper-bound i) 0)))
  (if (spans-zero? y)
	(error "Division error (interval spans 0):" y)
	(mul-interval
	  x
	  (make-interval (/ 1.0 (upper-bound y))
					 (/ 1.0 (lower-bound y))))))






 (define (eql-interval? a b) 
   (and (= (upper-bound a) (upper-bound b)) 
        (= (lower-bound a) (lower-bound b)))) 



 (define (ensure-mult-works aH aL bH bL) 
   (let ((a (make-interval aL aH)) 
         (b (make-interval bL bH))) 
   (if (eql-interval? (old-mul-interval a b) 
                      (mul-interval a b)) 
       true 
       (error "new mult returns different value!"  
              a  
              b  
              (old-mul-interval a b) 
              (mul-interval a b))))) 



 (ensure-mult-works  +10 +10   +10 +10) 
 (ensure-mult-works  +10 +10   +00 +10) 
 (ensure-mult-works  +10 +10   +00 +00) 
 (ensure-mult-works  +10 +10   +10 -10) 
 (ensure-mult-works  +10 +10   -10 +00) 
 (ensure-mult-works  +10 +10   -10 -10) 
  
 (ensure-mult-works  +00 +10   +10 +10) 
 (ensure-mult-works  +00 +10   +00 +10) 
 (ensure-mult-works  +00 +10   +00 +00) 
 (ensure-mult-works  +00 +10   +10 -10) 
 (ensure-mult-works  +00 +10   -10 +00) 
 (ensure-mult-works  +00 +10   -10 -10) 
  
 (ensure-mult-works  +00 +00   +10 +10) 
 (ensure-mult-works  +00 +00   +00 +10) 
 (ensure-mult-works  +00 +00   +00 +00) 
 (ensure-mult-works  +00 +00   +10 -10) 
 (ensure-mult-works  +00 +00   -10 +00) 
 (ensure-mult-works  +00 +00   -10 -10) 
  
 (ensure-mult-works  +10 -10   +10 +10) 
 (ensure-mult-works  +10 -10   +00 +10) 
 (ensure-mult-works  +10 -10   +00 +00) 
 (ensure-mult-works  +10 -10   +10 -10) 
 (ensure-mult-works  +10 -10   -10 +00) 
 (ensure-mult-works  +10 -10   -10 -10) 
  
 (ensure-mult-works  -10 +00   +10 +10) 
 (ensure-mult-works  -10 +00   +00 +10) 
 (ensure-mult-works  -10 +00   +00 +00) 
 (ensure-mult-works  -10 +00   +10 -10) 
 (ensure-mult-works  -10 +00   -10 +00) 
 (ensure-mult-works  -10 +00   -10 -10) 
  
 (ensure-mult-works  -10 -10   +10 +10) 
 (ensure-mult-works  -10 -10   +00 +10) 
 (ensure-mult-works  -10 -10   +00 +00) 
 (ensure-mult-works  -10 -10   +10 -10) 
 (ensure-mult-works  -10 -10   -10 +00) 
 (ensure-mult-works  -10 -10   -10 -10)