;(change-directory "~")
(change-directory "~/Documents/GitHub/data-pages-YPP2/06")

(load "../micro-graphics/load.lisp")
(load "../06.lisp")



(setf win (make-instance 'window))

(setf circ (make-instance 'circle))
(set-radius circ 100)

(set-shape win circ)
(redraw win)
(scale circ 1/2 (center circ))
(move circ 100 100)
;(redraw win)

(setf poly (make-instance 'polygon ))
(set-items poly (loop for (x . y) in '((0 . 0) (100 . 100) (100 . 0) (0 . 100))
                      collect (set-x (set-y (make-instance 'point) y) x)))
(set-filledp poly T)
(set-shape win poly)
(redraw win)


;;; vim: fenc=utf8
