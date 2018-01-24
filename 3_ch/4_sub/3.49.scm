;Consider the case where we have two classes of objects, a and b, where both may require the access to some of the other kind
;for certain operations. But to find out which, you first need access to the object itself. Then clearly it is
;possible for some process to grab an object of class a while some other process grab an object of class b - which
;just so happen to need access to each other for the completion of whatever calls were made. Besides, because of
;the to different classes, imposing an order may not be easy in any case - but with the acquisition needed before finding
;out which objects are further required, in any case impossible.
