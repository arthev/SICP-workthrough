(define (squarer a b)
  (multiplier a a b))

;Serious flaws: The multiplier won't set a when it only knows b, since it'll behave as if both a1 and a2 are unset.
;Thus, the logic module won't work in both directions, which is part of the point.
