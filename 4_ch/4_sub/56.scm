;;a - name of all people supervised by Ben Bitdiddle as well as their addresses
(and (supervisor ?person (Bitdiddle Ben))
	 (address ?person ?where))

;;b - all people whose salary is less than Bitdiddle's, together with their salary and Bitdiddle's.
(and (salary ?person ?person-salary)
	 (salary (Bitdiddle Ben) ?bit-salary)
	 (lisp-value > ?bit-salary ?person-salary))

;;c - all people who are supervised by someone who is not in the computer division,
;;    together with the supervisor's name and job

(and (supervisor ?supervised ?supervisor)
	 (not (job ?supervisor (computer . ?_)))
	 (job ?supervisor ?supervisor-job))
