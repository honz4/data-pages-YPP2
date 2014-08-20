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
                   (lambda (mgw) (declare (mgw ignore)) (redraw self)))
  self)

(defmethod initialize-instance ((self window) &key)
  (call-next-method) ;super
  (set-shape self (make-instance 'empty-shape))
  (install-callbacks self)
  self)


(let (w c)
  (setf w (make-instance 'window))
  (setf c (make-instance 'circle))
  (set-radius c 100)
  (set-y (set-x (center c) 100) 100) 
  (set-shape w c)
  (redraw w)
)
 ; vim: fenc=utf-8 syntax=lisp
