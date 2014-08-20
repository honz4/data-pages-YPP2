;======Priklad 9.9======
(change-directory "~/Documents/GitHub/data-pages-YPP2/09")

(load "../micro-graphics/load.lisp")
(load "../09.lisp")

(defclass test-delegate (mg-object) ())

(defmethod ev-change ((self test-delegate) sender message &rest args)
  (format t "change: sender=~s message=~s args=~s~%" sender message args)
  )

(let (d pt)
  (setf d (make-instance 'test-delegate))
  (setf pt (make-instance 'point))
  (set-delegate pt d)
  (set-events pt '(ev-change))

  (set-x pt 5) (sleep 1)
  (set-r pt 5) (sleep 1)
)
 ; vim: fenc=utf-8 syntax=lisp
