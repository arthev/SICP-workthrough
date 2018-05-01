(rule (big-shot ?person ?division)
	  (and (job ?person (?division . ?title))
		   (supervisor ?person ?boss)
		   (job ?boss (?boss-division . ?boss-title))
		   (not (same ?division ?boss-division))))

(rule (big-shot ?person ?division)
	  (and (job ?person (?division . ?ptitle))
		   (or (not (supervisor ?person ?boss))
			   (and (supervisor ?person ?boss)
					(not (same ?division ?boss-division))))))

;;Is the big boss a big-shot, or excluded? DUN DUNN DUNNNNN
