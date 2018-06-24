eval-dispatch
  ...
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  ...
...
ev-cond
  (assign exp (op cond->if) (reg exp))
  (goto (label eval-dispatch))

;;Of course, for this to work, the machine must have access to cond? and cond->if as primitives.
