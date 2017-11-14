#lang racket
(require sicp-pict)



(define identity (lambda (x) x))



(define (split first second)
  (let ((recurser ((lambda (x) (x x))
                   (lambda (split-gen)
                     (lambda (painter n)
                       (if (= n 0)
                           painter
                           (let ((smaller ((split-gen split-gen) painter (- n 1))))
                             (first painter (second smaller smaller)))))))))
    recurser))




;2.46
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
             (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1) (xcor-vect vect2))
             (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect scalar vect)
  (make-vect (* scalar (xcor-vect vect))
             (* scalar (ycor-vect vect))))

;2.47
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;(define (origin-frame frame)
;  (car frame))
;(define (edge1-frame frame)
;  (cadr frame))
;(define (edge2-frame frame)
;  (caddr frame))
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cddr frame))

;2.48
;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

;2.49
(define a (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0.99 0))
                                   (make-segment (make-vect 0.99 0) (make-vect 0.99 0.99))
                                   (make-segment (make-vect 0.99 0.99) (make-vect 0 0.99))
                                   (make-segment (make-vect 0 0.99) (make-vect 0 0)))))

(define b (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                                   (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define c (segments->painter (list (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
                                   (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
                                   (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
                                   (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0)))))
;2.52
(define right-split (split beside below))
(define up-split (split below beside)) 


(define crisscross (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0.99 0.99))
                                            (make-segment (make-vect 0 0.99) (make-vect 0.99 0))
                                            (make-segment (make-vect 0 0) (make-vect 0.99 0))
                                            (make-segment (make-vect 0.99 0) (make-vect 0.99 0.99))
                                            (make-segment (make-vect 0.99 0.99) (make-vect 0 0.99))
                                            (make-segment (make-vect 0 0.99) (make-vect 0 0))
                                            (make-segment (make-vect 0.5 0) (make-vect 0.5 0.99))
                                            (make-segment (make-vect 0 0.5) (make-vect 0.99 0.5)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

(paint (square-limit einstein 3))





















































































