(define (require p) (if (not p) (amb)))


(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define adjectives '(adjective green big saxon elephantish))

(define (parse-sentence)
  (list 'sentence
		(parse-noun-phrase)
		(parse-verb-phrase)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
		(parse-word prepositions)
		(parse-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
	(amb verb-phrase 
		 (maybe-extend
		   (list 'verb-phrase
				 verb-phrase
				 (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-adjectival-noun-phrase)
  (define (maybe-extend adjectival-phrase)
	(amb (list 'noun-phrase 
			   adjectival-phrase
			   (parse-word nouns))

		 (maybe-extend
		   (list 'adjectival-phrase
				 adjectival-phrase
				 (parse-word adjectives)))))
  (maybe-extend (parse-word articles)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
	(amb noun-phrase
		 (maybe-extend
		   (list 'noun-phrase
				 noun-phrase
				 (parse-prepositional-phrase)))))
  (maybe-extend (parse-adjectival-noun-phrase)))



(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
	(set! *unparsed* (cdr *unparsed*))
	(list (car word-list) found-word)))


(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
	(require (null? *unparsed*)) sent))
