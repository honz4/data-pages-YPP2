#|
exec sbcl --noinform --load $0 --end-toplevel-options "$@"
|#
;priklady/testy na geometrii, LOOP makro...
;list je sudy seznam x y ... (body pro polynom), vraci posunuto o x0 y0
(defun xy-move (list x0 y0)
  (loop with i = 1
        for x in list
        collect (if (oddp i) (+ x x0) (+ x y0))
        do (incf i)
        )
  )

;spocita n bodu na kruznici o polomeru r, phi0 = pi/2?
(defun xy-body-na-kruznici (n r phi0)
  (loop with phi = phi0 and step = (/ (* 2 pi) n)
        for i from 0 below n
        append (list (* r (cos phi)) (* r (sin phi)))
        do (incf phi step)
        )
  )

(format t "~A~%" (xy-move '(1 2 3 4) 10 100))
(format t "~A~%" (xy-body-na-kruznici 4 10 0))
