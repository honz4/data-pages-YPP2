;;==================================================
;; DEMONSTRACE


(defclass color-changing-window (window)
  ((chb :initform (move (make-instance 'item-list)  30 30))))


(defmethod chb ((ccw color-changing-window))
  (slot-value ccw 'chb))

(defmethod item-checked-chb1 ((ccw color-changing-window) sender origin val)
  (dolist (il (items sender))
    (if (checked il) (format T "~a, " (text il))))
  (format T "~%~%")
  )


(defmethod initialize-instance ((ccw color-changing-window)&key)
   (call-next-method)

   (let ((checkitems (mapcar #'make-checklist-item 
                             (list "yellow"
                                   "green"
                                   "red"
                                   "blue"
                                   "purple"
                                   "brown"))))

   (set-items (chb ccw) checkitems)

   (set-shape ccw (chb ccw))

   
   (add-event (chb ccw) '(ev-item-checked item-checked-chb1))

   ))

(make-instance 'color-changing-window)

