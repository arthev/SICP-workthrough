;4.2
;a)
;Louis' plan won't work. Application? is at the bottom of the eval
;cond because applications are left with the most general syntax. Eg. if something isn't anything else, we expect it to be an application.
;So, since (application? exp) currently invokes (pair? exp) this
;means that... just about everything in our language will be treated as applications. Whoops!

;b)
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))





