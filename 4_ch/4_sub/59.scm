;;a: Friday morning, Ben arrives, wants to query to see which meetings occur that day
(meeting ?where (Friday ?time))

;;b: understandable
(rule (meeting-time ?person ?day-and-time)
	  (and (job ?person (?department . ?r))
		   (or (meeting ?department ?day-and-time)
			   (meeting whole-company ?day-and-time))))

;;c:
(meeting-time (Hacker Alyssa P) (Wednesday ?t))

;;As found online: If the meeting place is relevant...
(and (meeting-time (Hacker Alyssa P) (Wednesday ?t))
	 (meeting ?dept (Wednesday ?t)))
;;May be better...
