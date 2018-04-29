(define (require p) (if (not p) (amb)))

(define (an-element-of l)
  (define (iter wl)
	(if (null? (cdr wl))
	  (car wl)
	  (ramb (car wl) (iter (cdr wl)))))
  (iter l))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

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

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
		(parse-word articles)
		(parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
	(amb noun-phrase
		 (maybe-extend
		   (list 'noun-phrase
				 noun-phrase
				 (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
	(set! *unparsed* (cdr *unparsed*))
	(list (car word-list) found-word)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (set! *unparsed* (cdr *unparsed*))
  (list (car word-list) (an-element-of (cdr word-list))))



(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
	(require (null? *unparsed*)) sent))



;;Note: This program does not generate all possible sentences over the grammar.
;;Also my initial approach fell into infinite loops, since the method of termination
;;here is to exhaust *unparsed*. So after some thinking I looked online to see
;;if I was on the right track and discovered that yes, I was, except for the lack
;;of termination. But it doesn't generate all possible sentences over the grammar,
;;which was how I read the exercise task. Mildly disappointed. Still kinda neat
;;to see the effects of such a small change, though. Anyhow, currently it terminates
;;as it exhausts one of the word lists. Probably the articles, heh.
;;
;;Using *unparsed* as termination doesn't seem like ignoring the "input sentence" to me!
