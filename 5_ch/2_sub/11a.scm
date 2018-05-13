;Simply replace
...
afterfib-n-2
  (assign n (reg vall))
  (restore val)
  (restore c)
  (assign val (op +) (reg val) (reg n))
  ...
...

;with
...
afterfib-n-2
  (restore n)
  (restore c)
  (assign val (op +) (reg val) (reg n))
  ...
...

;This is because we want the sum of old-val and val into val.
;In the original, we switch the n <- val, and then restore val.
;But since there's no weird leakages, we can just restore the old-val
;to n instead and accomplish the same.
