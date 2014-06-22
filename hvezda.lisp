(defun mp (x y)
  (set-y (set-x (make-instance 'point) x) y))
     
(defun hvezda (point size)
  (let (result)
    (dotimes (i 12)
      (setf result 
            (cons 
              (rotate (mp (+  (if (oddp i) size (/ size 2))
                              (x point))
                          (y point))

                      (* pi 1/6 i)
                      point)
              result)))
    (set-items (make-instance 'polygon) result)))


                  
