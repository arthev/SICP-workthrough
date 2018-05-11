;;Original:
(controller
  ...
  afterfib-n-1
    (restore n)
	(restore continue)
	(assign n (op -) (reg n) (const 2))
	(save continue)
	...
  ...
  )

;;Improved by removing extraneous save and restore:
(controller
  ...
  afterfib-n-1
    (restore n)
	(assign n (op -) (reg n) (const 2))
	...
  ...
  )
