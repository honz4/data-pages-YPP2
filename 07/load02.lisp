;(change-directory "~")
(change-directory "~/Documents/GitHub/data-pages-YPP2/07")

(load "../micro-graphics/load.lisp")
(load "../07.lisp")


(setf win (make-instance 'window))
;(set-x (set-x win 500) 300)

(setf pic (make-instance 'picture))
(set-shape win pic)


;   _______
;   ___0___
;| |       | |
;|1|       |2|
;| |       | |
;| |       | |
;   _______
;   ___3___
;| |       | |
;|4|       |5|
;| |       | |
;| |       | |
;   _______
;   ___6___
(set-items pic
;posunuti a pootoceni jednotlivych segmentu
;                   0   1   2   3   4   5   6
(loop with pi2 = (/ pi 2)
      for dx in '(     0   0  80   0   0  80   0)
      for dy in '(     0   0   0  80  80  80  160)
      for fi in (list  0 pi2 pi2   0 pi2 pi2   0)
  collect
  (let ((points (loop for x in '(10 20 80 90 80 20);souradnice segmentu
		      for y in '(10  0  0 10 20 20)
		  collect (set-x (set-y (make-instance 'point) y) x))))
	(move
        (rotate
	(set-items (make-instance 'polygon) points)
	 fi (car points))
         dx dy)
)))

(redraw win)
#|
|#
