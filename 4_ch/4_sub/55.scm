;;1 - all people supervised by Ben
(supervisor ?person (Bitdiddle Ben))

;;2 - names and jobs of all people in the accounting division
(job ?person (accounting . ?title))

;;3 - names and addresses of all people who live in Slumerville
(address ?name (Slumerville ?street ?number))
