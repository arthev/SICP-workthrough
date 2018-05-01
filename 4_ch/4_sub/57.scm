;;Define a rule can-replace so that p1 can replace p2 if they do the same job,
;;or p1 can-do-job p2, and p1 and p2 are not the same person.

(rule (can-replace ?person1 ?person2)
	  (and  
		(or 
		  (and
			(job ?person1 ?job1)
			(job ?person2 ?job2)
			(same ?job1 ?job2))
		  (can-do-job ?job1 ?job2))
		(not (same ?person1 ?person2))))

;;The above probably works, but it's clumsy. Below definition from the web is better:
(rule (can-replace ?person1 ?person2)
	  (and (job ?person1 ?job1)
		   (job ?person2 ?job2)
		   (or (same ?job1 ?job2)
			   (can-do-job ?job1 ?job2))
		   (not (same ?person1 ?person2))))

;;everyone who can replace Cy D. Fect
(can-replace ?person (Fect Cy D))


;;all people who can replace someone being paid more than they are, together with the two salaries
(and (can-replace ?person1 ?person2)
	 (salary ?person1 ?salary1)
	 (salary ?person2 ?salary2)
	 (lisp-value > ?salary2 ?salary1))
