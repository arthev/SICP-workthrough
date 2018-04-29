;;; Amb-Eval input:
(parse '(the professor lectures to the student in the class with the cat))

;;; Starting a new problem 
;;; Amb-Eval value:
(sentence 
  (simple-noun-phrase 
	(article the) 
	(noun professor)) 
  (verb-phrase 
	(verb-phrase 
	  (verb-phrase 
		(verb lectures) 
		(prep-phrase 
		  (prep to) 
		  (simple-noun-phrase 
			(article the) 
			(noun student)))) 
	  (prep-phrase 
		(prep in) 
		(simple-noun-phrase 
		  (article the) 
		  (noun class)))) 
	(prep-phrase 
	  (prep with) 
	  (simple-noun-phrase 
		(article the) 
		(noun cat)))))
;;The professor LECTURES to the student who is in a class wherein there is a cat. Focus on there being a cat there.


;;; Amb-Eval value:
(sentence 
  (simple-noun-phrase 
	(article the) 
	(noun professor)) 
  (verb-phrase 
	(verb-phrase 
	  (verb lectures) 
	  (prep-phrase 
		(prep to) 
		(simple-noun-phrase 
		  (article the) 
		  (noun student)))) 
	(prep-phrase 
	  (prep in) 
	  (noun-phrase 
		(simple-noun-phrase
		  (article the) 
		  (noun class)) 
		(prep-phrase 
		  (prep with) 
		  (simple-noun-phrase 
			(article the) 
			(noun cat)))))))
;; The professor LECTURES *in* the class where there's a cat, and he lectures TO the student


;;; Amb-Eval value:
(sentence 
  (simple-noun-phrase 
	(article the)
	(noun professor)) 
  (verb-phrase 
	(verb-phrase 
	  (verb lectures) 
	  (prep-phrase 
		(prep to) 
		(noun-phrase 
		  (simple-noun-phrase 
			(article the)
			(noun student)) 
		  (prep-phrase 
			(prep in) 
			(simple-noun-phrase 
			  (article the) 
			  (noun class)))))) 
	(prep-phrase 
	  (prep with) 
	  (simple-noun-phrase 
		(article the) 
		(noun cat)))))
;;The professor LECTURES to the student who is in the class, and the professor uses a cat for the lecture.

;;; Amb-Eval value:
(sentence 
  (simple-noun-phrase 
	(article the) 
	(noun professor)) 
  (verb-phrase 
	(verb lectures) 
	(prep-phrase 
	  (prep to)
	  (noun-phrase 
		(noun-phrase 
		  (simple-noun-phrase 
			(article the) 
			(noun student)) 
		  (prep-phrase 
			(prep in) 
			(simple-noun-phrase 
			  (article the) 
			  (noun class)))) 
		(prep-phrase 
		  (prep with) 
		  (simple-noun-phrase 
			(article the)
			(noun cat)))))))
;;The professor LECTURES to the student who is in the class and who is in the class with a cat (probably brought his cat to the class?)

;;; Amb-Eval value:
(sentence 
  (simple-noun-phrase 
	(article the) 
	(noun professor)) 
  (verb-phrase 
	(verb lectures) 
	(prep-phrase 
	  (prep to) 
	  (noun-phrase 
		(simple-noun-phrase 
		  (article the) 
		  (noun student)) 
		(prep-phrase 
		  (prep in) 
		  (noun-phrase 
			(simple-noun-phrase 
			  (article the) 
			  (noun class)) 
			(prep-phrase 
			  (prep with) 
			  (simple-noun-phrase 
				(article the) 
				(noun cat)))))))))
;;The professor LECTURES to the student who is in the class where there's a cat. Focus on lecturing.





