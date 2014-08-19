(defclass message-inverter ()
  (sender))

(defmethod inv-sender ((inv message-inverter))
  (slot-value inv 'sender))

(defmethod invert ((inv message-inverter) message sender args)
  (setf (slot-value inv 'sender) sender)
  (apply message inv args))

(defmethod move ((inv message-inverter) dx dy)
  (list 'move (inv-sender inv) (- dx) (- dy)))

(defmethod set-color ((inv message-inverter) color)
  (list 'set-color (inv-sender inv) (color (inv-sender inv))))

(defclass undo-window (window)
  ((inverter :initform (make-instance 'message-inverter))
   (undo-stack :initform '())))

(defmethod set-shape ((w undo-window) shape)
  (call-next-method)
  (add-event shape 'ev-changing)
  w)

(defmethod ev-changing ((w undo-window) sender message &rest args)
  (setf (slot-value w 'undo-stack)
        (cons (invert (slot-value w 'inverter) message sender args)
              (slot-value w 'undo-stack)))
  w)

(defmethod undo ((w undo-window))
  (without-changes (shape w)
    (let ((top (car (slot-value w 'undo-stack))))
      (apply (car top) (cdr top))))
  (setf (slot-value w 'undo-stack)
        (cdr (slot-value w 'undo-stack)))
  (invalidate w))