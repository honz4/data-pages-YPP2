;(change-directory "~")
(change-directory "~/Documents/GitHub/data-pages-YPP2/06")

(load "../micro-graphics/load.lisp")
(load "../06.lisp")
(load "../hvezda.lisp")

(defvar win) (setf win (make-instance 'window))

(defvar hvezda) (setf hvezda  (hvezda (mp 100 100) 100))
(set-shape win hvezda)
;(redraw win) (sleep 1)

(defun mp (x y) (set-y (set-x (make-instance 'point) x) y))

(defparameter *w* 600)
(defparameter *h* 400)
;okno bude 250x200, ale pouzijeme pristup pomoci slot-value,
;window nema set-mg-window metodu
(let ((mgw (mg-window win)))
  (when mgw
    (progn
    (mg:close-window mgw)
    (setf (slot-value win 'mg-window) (mg:display-window :width *w* :height *h*))
    )))
;(set-share win hvezda);netreba?
(redraw win)

(set-background win :red)

(defvar items) (setf items nil)
(defvar pic) (setf pic (make-instance 'picture))
(setf poly (set-color (make-instance 'polygon) :green))
(set-items poly
		  (loop for (x . y) in (list (cons 0 0) (cons *w* *h*)
		                             (cons *w* 0) (cons 0 *h*))
                   do (format t "~a~%" (cons x y))
		   collect (set-y (set-x (make-instance 'point) x) y)))
(push (set-closedp (set-filledp poly T) T) items)
;dve bile uhlopricky: 
(push (set-thickness
      (set-color
      (set-items (make-instance 'polygon) (list (mp 0 0) (mp *w* *h*)))
      :white) (/ *h* 6))
items)


(push (let ((poly (make-instance 'polygon)))
  (set-color poly :white)
  (set-items poly (list (mp *w* 0) (mp 0 *h*)))
  (set-thickness poly (/ *h* 6))
  poly
) items)

;bily kruh uprostred:
(push (move (set-filledp (set-color (set-radius (make-instance 'circle)
     (/ *w* 4)) :white) t) (/ *w* 2) (/ *h* 2))
items)

(loop with r = (/ *h* 6)
      for phi from (- (/ pi 2)) to pi by (/ (* 2 pi) 3)
do (let ((hvezda (hvezda (mp (* (cos phi) r) (* (sin phi) r))  (/ *h* 8)) ))

(push (set-filledp (set-color (move hvezda (/ *w* 2) (/ *h* 2)) :red) t) items)
;kopie bodu polygonu, novy polygon, okraj hvezdy
(push
(set-color (set-filledp (set-thickness
(set-items (make-instance 'polygon )
     (loop for p in (items hvezda) collect (mp (x p) (y p)) ))
                4) nil) :green)
items)
))


(set-items pic items)
(set-shape win pic)
(redraw win)
