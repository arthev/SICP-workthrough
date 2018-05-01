;;Attempt uno:
(rule (last-pair (?z) (?z)))
(rule (last-pair (?x . ?y) (?z))
	  (last-pair ?y (?z)))

;Works for the three queries to check against. Doesn't work
;for (last-pair ?x (3)). Probably because of the infinite number
;of potential solutions. Exceeds recursion depth.
