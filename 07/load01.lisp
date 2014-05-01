(load "../micro-graphics/load.lisp")
(load "../07.lisp")

(setf win (make-instance 'window))

(setf circ (make-instance 'circle))
(set-radius circ 100)

(set-shape win circ)
(redraw win)
(scale circ 1/2 (center circ))
(move circ 100 100)
#|
|#
