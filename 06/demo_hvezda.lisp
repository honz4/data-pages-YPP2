;(change-directory "~")
(change-directory "~/Documents/GitHub/data-pages-YPP2/06")

#|
(defparameter *hh* 400)
(loop with r = (/ *hh* 3)
      for phi from (/ pi 2) to (* pi 2) by (/ pi 3)
do (format t "~a~%" (list r phi))
)
|#

(load "../micro-graphics/load.lisp")
(load "../06.lisp")
(load "../hvezda.lisp")

(setf win (make-instance 'window))

(set-shape win (hvezda (mp 100 100) 100))

(redraw win)
