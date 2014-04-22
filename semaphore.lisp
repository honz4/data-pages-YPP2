
(defclass traffic-light (circle)
  ((on :initform NIL)
   (on-color :initform :GREEN)
   (off-color :initform :GRAY)))

(defmethod initialize-instance ((self traffic-light) &key)
  (call-next-method)
  (set-radius self 25)
  (set-filledp self T)
  (ensure-color self))

(defmethod on ((self traffic-light))
  (slot-value  self 'on))

(defmethod set-on ((self traffic-light) val)
  (setf (slot-value  self 'on) val)
  (ensure-color self)
  self)


(defmethod on-color ((self traffic-light))
  (slot-value  self 'on-color))

(defmethod off-color ((self traffic-light))
  (slot-value  self 'off-color))

(defmethod ensure-color ((self traffic-light))
        (set-color self 
                 (if (on self)
                     (on-color self)
                   (off-color self))))

(defmethod set-on-color ((self traffic-light) val)
  (setf (slot-value  self 'on-color) val)
  (ensure-color self)
  self)

(defmethod set-off-color ((self traffic-light) val)
  (setf (slot-value  self 'off-color) val)
  (ensure-color self)
  self)

;;--------------------------------------------


(defvar *program* '((T NIL NIL) 
                    (T T   NIL)
                    (NIL NIL T)
                    (NIL T NIL)))

(defun mp (x y)
  (move (make-instance 'point) x y))

(defclass semaphore (picture)
  ((state :initform 0)
   (lights :initform NIL)))

(defmethod state ((self semaphore))
  (slot-value self 'state))

(defmethod lights ((self semaphore))
  (slot-value self 'lights))


(defmethod set-lights ((self semaphore) val)
  ;; tady by spravne mel byt test kvuli zapouzdreni
  ;; jestli je opravdu predavan seznam svetel
  ;; kvuli rychlosti nedelam
  (setf (slot-value self 'lights) val)
  self
)



(defmethod set-state ((self semaphore) val)

  (with-change (self 'set-state val)

    (setf (slot-value self 'state) val)
 
    (mapcar 
     (lambda (x l)
       (set-on l x))
   
     (nth val *program*)
     (lights self)))

    self)

(defmethod initialize-instance ((self semaphore) &key)
  (call-next-method)
  (let ((bounding-rectangle
         (set-items (make-instance 'polygon)
                    (list (mp -30 -30)
                          (mp 30 -30)
                          (mp 30 175)
                          (mp -30 175))))) 
    (set-filledp bounding-rectangle T)
    (set-color bounding-rectangle :BLACK)
         
    (set-lights self (list (set-on-color (make-instance 'traffic-light) :RED)
                           (move (set-on-color (make-instance 'traffic-light) :ORANGE) 0 60)
                           (move (set-on-color (make-instance 'traffic-light) :GREEN) 0 120)))

    (set-items self
               `(,@(lights self)
                 ,bounding-rectangle))

    (move self 30 30)))


(defmethod next ((self semaphore))
  (set-state self (mod (+ 1 (state self)) (length *program*))))

;;--------------------------------------------

(defclass picture-with-semaphore-and-button
          (picture)
  ((semaphore :initform (make-instance 'semaphore))
   (button :initform (make-instance 'polygon))))

(defmethod button ((self picture-with-semaphore-and-button))
  (slot-value self 'button))

(defmethod semaphore ((self picture-with-semaphore-and-button))
  (slot-value self 'semaphore))


(defmethod initialize-instance ((self picture-with-semaphore-and-button) &key)

  (call-next-method)

    
  (set-items (button self) (list (mp -20 -10)
                                 (mp 20 -10)
                                 (mp 20 10)
                                 (mp -20 10)))

  (move (button self) 100 50)
  (set-filledp (button self) T)
  (set-color (button self) :GRAY)
  (set-items self (list (semaphore self) (button self))))


(defmethod initialize-events ((self picture-with-semaphore-and-button))
  (add-event (button self) '(ev-mouse-down button-clicked))
  self)

(defmethod button-clicked ((self picture-with-semaphore-and-button) sender origin button point)
  (next (semaphore self)))

;;--------------------------------------------


(set-shape 
 (make-instance 'window)
 (initialize-events (make-instance 'picture-with-semaphore-and-button)))
