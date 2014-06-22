(load "micro-graphics/load.lisp")

(defun burundi (&key (H 300) (W 600))

  (let ((win (mg:display-window :height H :width W))
        )
    (mg:draw-circle win 20 20 30)

    (format t "bg: ~a" (mg:get-param win :background))

    (mg:set-param win :background :green)
    (mg:clear win)
    (mg:set-param win :background :red)
    (mg:draw-circle win 10 10 10)

win ;return
    ))
