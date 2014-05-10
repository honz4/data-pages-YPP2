;(change-directory "~")
(change-directory "~/Documents/GitHub/data-pages-YPP2/07")

(load "../micro-graphics/load.lisp")
(load "../07.lisp")

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

(setf win (make-instance 'window))

(setf pic (make-instance 'picture))
(set-shape win pic)


(defvar pi2 1.57) ;pi pul, pootoceni o 90stupnu

(set-items pic
;posunuti a pootoceni jednotlivych segmentu
;                   0   1   2   3   4   5   6
(loop for dx in '(  0   0 100   0   0 100   0)
      for dy in '(  0   0   0 100 100 100 100)
      for fi in '(  0 pi2 pi2   0 pi2 pi2   0)
  collect
(move
(set-items 
  (make-instance 'polygon)
  (loop for x in '( 0 10 90 100 90 10  0);souradnice segmentu
        for y in '(10  0  0  10 20 20 10)
        collect (set-x (set-y (make-instance 'point) y) x))
)
dx dy)
))

(redraw win)
#|
|#
