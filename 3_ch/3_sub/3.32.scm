;The simulator propogates values along the wires and through the chips.
;When an event is triggered, it gets added to the agenda queues.
;This means that the events get added to the agenda queues in the order which
;they occur! And because of FIFO, get executed in the order they are triggered too!
;In other words, using the queues ensures proper logic in continuous time,
;for the times inbetween the discrete time the agenda itself handles by its time lookups.
;
;So if we had used a LIFO list instead, events would happen in reverse order.
;So if e.g. something makes a signal in FIFO change to 0 and then to 1, it will
;instead be changed to 1 and then 0 in the LIFO.
;
;I have trouble understanding exactly how things change in the example the book asks us to trace.
;Assumed interpretation: the and-gate's inputs change from X,X to 0,1 to 1,0 in the same segment.
;But what happens depends on whether the inputs change at the exact same time (maybe being influenced
;by some stuff rather than being direct inputs). Anyway, the general behaviour of FIFOs and LIFOs is clear.
;(The and-gate will probably blast 1 for a moment while the inputs change slightly asynchronously, if they
;change one at a time. But the ending-states will differ from FIFO and LIFO.)
