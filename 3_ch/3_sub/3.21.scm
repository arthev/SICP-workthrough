(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
	(error "FRONT called with an empty queue" queue)
	(car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
	(cond ((empty-queue? queue) 
		   (set-front-ptr! queue new-pair)						
		   (set-rear-ptr! queue new-pair)						
		   queue)
		  (else
			(set-cdr! (rear-ptr queue) new-pair)
			(set-rear-ptr! queue new-pair)
			queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
		 (error "DELETE called with an empty queue" queue))
		(else (set-front-ptr! queue (cdr (front-ptr queue)))
			  queue)))

;3.21
(define (print-queue queue)
	(newline)
	(display (front-ptr queue)))
	(newline)

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(delete-queue! q1)
(print-queue q1)

;The SCHEME printer thinks the queue is a simple list, since the last item in the queue, the rear-ptr, leads into
;ending in a pair (item '()), which terminates lists.
;Anyhow, the queue is a pair containing two items, two pointers, and when you add something to the queue,
;then it's inserted at the end, and will thus 'look' like it's been added twice since the rear-ptr gets updated
;to point to it, so we can have that theta(1) fun. 
