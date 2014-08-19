;; -*- mode: lisp; encoding: utf-8; -*-

(defun random-color () 
  (let ((colors (color:get-all-color-names))) 
    (nth (random (length colors)) colors)))

(defclass click-circle (circle) ())

(defmethod mouse-down ((circ click-circle) button position) 
  (set-color circ (random-color)) 
  ;; Oznámení o kliknutí delegátovi (umožňuje použít klikací kolečko
  ;; současně k jiným účelům):
  (call-next-method))
          
(defun make-point (x y)
  (move (make-instance 'point) x y))

(defun make-polygon (coord-list filledp closedp color)
  (set-closedp (set-filledp
                (set-color
                 (set-items (make-instance 'polygon)
                            (mapcar (lambda (pair)
                                      (apply #'make-point pair))
                                    coord-list))
                 color)
                filledp)
               closedp))

(defun make-arr (color)
  (make-polygon '((0 -30) (0 -15) (30 -15) (30 15) (0 15) (0 30) (-30 0))
                t
                t
                color))

