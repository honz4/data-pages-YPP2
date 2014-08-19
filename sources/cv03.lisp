;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Kód z cvièení PP2I, 13. bøezna 2008
;;;;

(defclass house ()
  ((rooms :initform '())
   current-room))

(defclass place ()
  (name neighbors))

(defun make-place (name)
  (let ((place (make-instance 'place)))
    (setf (slot-value place 'name) name)
    place))

(defun make-house ()
  (let ((house (make-instance 'house))
        (entrance (make-place 'entrance))
        (kitchen (make-place 'kitchen))
        (cellar (make-place 'cellar)))
    (setf (slot-value entrance 'neighbors) (list kitchen cellar)
          (slot-value kitchen 'neighbors) (list entrance)
          (slot-value cellar 'neighbors) (list entrance)
          (slot-value house 'rooms) (list kitchen cellar entrance)
          (slot-value house 'current-room) entrance)
    house))