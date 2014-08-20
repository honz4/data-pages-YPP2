;======Priklad 9.9======
(change-directory "~/Documents/GitHub/data-pages-YPP2/09")

(load "../micro-graphics/load.lisp")
(load "../09.lisp")

(let (w circle)
  (setf w (make-instance 'window))
  (setf circle (make-instance 'circle))
  (set-shape w circle)
  (sleep 2)

  (set-radius circle 40) (sleep 1)
  (set-filledp circle t) (sleep 1)
  (set-color circle :red) (sleep 1)
  (move circle 100 100)    (sleep 1)
  (set-background w :blue) (sleep 1)

  (move (center circle) 0 -50)
)
 ; vim: fenc=utf-8 syntax=lisp
