(controller
    (assign p (const 1))
	(assign c (const 1))
  test-c
    (test (op >) (reg c) (reg n))
	(branch (label fact-done))
	(assign p (op *) (reg p) (reg c))
	(assign c (op +) (reg c) (const 1))
	(goto (label test-c))
  fact-done)
