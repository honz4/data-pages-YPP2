;(load "micro-graphics/load.lisp")

(defun burundi (&key (h 300) (w 400))
(let (win)
 (setf win (mg:display-window :height h :width w))
 (mg:set-param win :background :green)
 (mg:clear win)

 (mg:set-param win :foreground :red)
 (mg:set-param win :closedp t)
 (mg:set-param win :filledp t)
 (mg:draw-polygon win (list 0 0  w h  w 0  0 h))

 (mg:set-param win :foreground :white)
 (mg:set-param win :closedp nil)
 (mg:set-param win :filledp nil)

 (mg:set-param win :thickness (/ h 6))
 (mg:draw-polygon win (list 0 0 w h))
 (mg:draw-polygon win (list w 0 0 h))

 (mg:set-param win :filledp t)
 (mg:draw-circle win (/ w 2) (/ h 2) (/ h 3))
 win ;return
)
)

(defun star (&key (r 20) (n 6))
  
)
