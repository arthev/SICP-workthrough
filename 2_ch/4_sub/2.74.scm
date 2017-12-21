;a) Assuming that changes have been made to use a data-directed approach, and that
;all 'files' now specify what division they belong to/are from, as a tag. Eg. (division file), 
;the get-record function simply needs to do a lookup in a table and then delegate appropriately.
(define (get-record employee tagged-file)
  ((get (division-tag tagged-file) 'get-record) employee (division-file tagged-file)))
;This assumes the table is implemented with division-tag as first axis and function as second axis,
;and that all such get-record functions share the same name, as well as the parameter ordering.


;b) The easiest option here is to have any sender of a record tag the record with the division, 
;again, thus making a structure like (division record). It's not reasonable to assume anything
;about the structure of the record itself, because they might use pairs, or lists, or binary trees,
;or some other structure. Therefore, we delegate in much the same way as in a).
(define (get-salary tagged-record)
  ((get (division-tag tagged-record) 'get-salary) (division-record tagged-record)))
;This assumes similar installation procedure, as per the short introduction in the book,
;as used in a). As well as the same assumptions re. number of parametres etc.


;c) Harder to say what might be reasonable assumptions in this subtask. However,
;as the HQ of Insatiable Enterprises, Inc, we can surely demand certain actions from
;our different divisions. Therefore, we demand that all divisions install a find-employee?
;function that checks whether that employee is in a file (using the division's format, of course),
;returning either #t or #f, depending on whether the employee was found.
(define (find-employee-record employee list-of-tagged-files)
  (let ((current-division-tagged-file (car list-of-tagged-files))
		(other-division-files (cdr list-of-tagged-files)))
	(let ((current-division-tag (division-tag current-division-tagged-file))
		  (current-division-file (division-file current-division-tagged-file)))
	  (let ((find-employee? (get current-division-tag 'find-employee?)))
		(if (find-employee? current-division-file)
		  (get-record employee current-division-tagged-file)
		  (find-employee-record employee other-division-files))))))
;Standard recursive solution, checking whether any given file contains the employee,
;and, if so, delegating to the get-record function.


;d) The new division will need to implement an interface so that the HQ operations
;will properly delegate to the relevant functions from that division. That is, the
;new division will need to make an install package that inserts relevant data into the
;data-directed look-up table.
