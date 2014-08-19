;======Priklad 9.2======
(change-directory "~/Documents/GitHub/data-pages-YPP2/09")

(load "../micro-graphics/load.lisp")
(load "../09.lisp")

 ;(set-callback mg-window callback-name function|nil) => nil
 ;(get-callback mg-window callback-name ) => function|nil
 ; :display     (lambda (mgw) (declare (ignore mgw))   ...)
 ; :mouse-down  (lambda (mgw :left|:center|:right x y) ...)

(defmethod install-callbacks ((self window))
  (mg:set-callback (slot-value self 'mg-window)
                   :display
                   (lambda (mgw) (declare (mgw ignore)) redraw))
  )

(defmethod initialize-instance ((self window) &key)
  (call-next-method) ;super
  (set-shape self (make-instance 'empty-shape))
  (install-callbacks self)
  )


(let (w)
  (setf w (make-instance 'window))
  (set-shape w (make-instance 'circle (set-y (set-x (make-instance 'point) 100) 100) 100))
  (redraw w)
)
 ; vim: fenc=utf-8 syntax=lisp
