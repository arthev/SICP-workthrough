;a) the deriv program aws changed so that it utilises a data-driven table, where the functions to call are looked up in a table dependent on the type of mathematical operation an exp is.
;number? and variable? could not be trivially set up to use the  data-driven dispatch, because those rely on different
;methods of identification than the 'tag' mathematical operator of, say, (* x x).
;We *can* assimilate handling of those into the data-driven dispatch too... but then we need to add extra tags, so that a number is no longer f, but instead (list 'num 4), for example.


;b) (and c)!)

(define (install-deriv-package)
  ;; internal procedures
  (define (sum-deriv ops var)
	(make-sum (deriv (car ops) var)
			  (deriv (cadr ops) var)))
  (define (product-deriv ops var)
	(make-sum (make-product (car ops)
							(deriv (cadr ops) var))
			  (make-product (deriv (car ops) var)
							(cadr ops))))
  (define (exp-deriv ops var)
	(make-product (cadr ops) 
				  (make-product (deriv (car ops) var)
								(make-exp (car ops) (- (cadr ops) 1)))))

  ;;interface to the rest of the system
  (put '(deriv) '+ sum-deriv)
  (put '(deriv) '* product-deriv)
  (put '(deriv) '** exp-deriv)


  'done)

;d) Changing the 'order' of dispatch means that attempts to look up functions in the data-dispatch table now
;follows a different ordering. The only significant difference necessary to cope with this is a different installation process.
;Aka different use of puts and gets. Aka different order.
