;;vraci seznam desitkovych cislic
(defun digits (n &optional (radix 10) (digits nil))
  (if (< n radix)
     (cons n digits)
     (multiple-value-bind (n d) (floor n radix)
       (digits n radix (cons d digits)))))

#|
|#
(format t "~a~%" (digits 79807))
