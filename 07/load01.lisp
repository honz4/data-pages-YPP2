;(change-directory "~")
(change-directory "~/Documents/GitHub/data-pages-YPP2/07")

(load "../micro-graphics/load.lisp")
(load "../07.lisp")

(setf win (make-instance 'window))

(setf circ (make-instance 'circle))
(set-radius circ 100)

(set-shape win circ)
(redraw win)
(scale circ 1/2 (center circ))
(move circ 100 200)
(redraw win)

(setf poly (make-instance 'polygon))
(setf p1 (make-instance 'point))
(setf p2 (make-instance 'point))
(setf p3 (make-instance 'point))
(setf p4 (make-instance 'point))
(setf p5 (make-instance 'point))
(setf p6 (make-instance 'point))
(set-y (set-x p1  10)  0)
(set-y (set-x p2  90)  0)
(set-y (set-x p3 100) 10)
(set-y (set-x p4  90) 20)
(set-y (set-x p5  10) 20)
(set-y (set-x p6  0)  10)
(set-items poly (list p6 p1 p2 p3 p4 p5))
;udelame pootocene/posunute kopie: pozor, polygon items porad stejne body!
(setf poly2 (make-instance 'polygon))
(set-items poly2 (items poly))
(move (rotate poly2 1.57 (car (items poly2))) 100 0)

(setf pic (make-instance 'picture))
(set-items pic (list poly poly2))
(set-shape win pic)
(redraw win)

#|
(defvar pi2 1.57)
;                   0   1   2   3   4   5   6
(loop for dx in '(  0   0 100   0   0 100   0)
      for dy in '(  0   0   0 100 100 100 100)
      for fi in '(  0 pi2 pi2   0 pi2 pi2   0)
  collect
|#
(setf poly3 (make-instance 'polygon))
(set-items poly3
     (loop for x in '( 0 10 90 100 90 10  0)
           for y in '(10  0  0  10 20 20 10)
       collect (set-x (set-y (make-instance 'point) y) x))
)
(set-items pic (list poly2 poly3))
(redraw win)
#|
|#
